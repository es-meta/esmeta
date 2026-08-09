package esmeta.analyzer.tychecker

import esmeta.ir.*
import esmeta.ty.{*, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** abstract states */
trait AbsStateDecl { self: TyChecker =>
  import tyStringifier.given

  case class AbsState(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    symEnv: Map[Sym, ValueTy],
    constr: TypeConstr,
  ) extends AbsStateLike {
    import AbsState.*

    given AbsState = this

    /** bottom check */
    def isBottom: Boolean = !reachable || locals.values.exists(_.isBottom)

    /** partial order */
    def ⊑(that: AbsState): Boolean = (this, that) match
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (
            AbsState(_, llocals, lsymEnv, lconstr),
            AbsState(_, rlocals, rsymEnv, rconstr),
          ) =>
        llocals.forall { (x, lv) =>
          rlocals.get(x).fold(false) { rv => (lv, this) ⊑ (rv, that) }
        } &&
        lsymEnv.forall { (sym, ty) => rsymEnv.get(sym).fold(false)(ty <= _) } &&
        lconstr <= rconstr

    /** not partial order */
    def !⊑(that: AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (l, r) =>
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = (l.get(x), l) ⊔ (r.get(x), r)
        } yield x -> v).toMap
        val newSymEnv = (for {
          sym <- (l.symEnv.keySet ++ r.symEnv.keySet).toList
          ty = l.get(sym) || r.get(sym)
        } yield sym -> ty).toMap
        val newConstr = l.constr || r.constr
        AbsState(true, newLocals, newSymEnv, newConstr)

    /** copy operator */
    def copy(
      reachable: Boolean = reachable,
      locals: Map[Local, AbsValue] = locals,
      symEnv: Map[Sym, ValueTy] = symEnv,
      constr: TypeConstr = constr,
    ): AbsState = AbsState(reachable, locals, symEnv, constr)

    /** get imprecise bases compared with another state */
    def getImprecBases(that: AbsState): Set[Base] =
      val locals = (for {
        (x, lv) <- this.locals
        if that.locals.get(x) match
          case None     => true
          case Some(rv) => !(rv.ty(using that) <= lv.ty(using this))
      } yield x).toSet
      val syms = (for {
        (x, lty) <- this.symEnv
        if that.symEnv.get(x) match
          case None      => true
          case Some(rty) => !(rty <= lty)
      } yield x).toSet
      locals ++ syms

    /** meet operator */
    def ⊓(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (l, r) =>
        val newLocals = (for {
          x <- (l.locals.keySet intersect r.locals.keySet).toList
          v = (l.get(x), l) ⊓ (r.get(x), r)
        } yield x -> v).toMap
        val newSymEnv = (for {
          sym <- (l.symEnv.keySet intersect r.symEnv.keySet).toList
          ty = l.get(sym) && r.get(sym)
        } yield sym -> ty).toMap
        val newConstr = l.constr && r.constr
        AbsState(true, newLocals, newSymEnv, newConstr)

    /** kill bases */
    def kill(bases: Set[Base], update: Boolean): AbsState =
      val newLocals = for { (x, v) <- locals } yield x -> v.kill(bases, update)
      val newConstr = if (update) constr.kill(bases) else constr
      AbsState(reachable, newLocals, symEnv, newConstr)

    /** has imprecise elements */
    def hasImprec: Boolean = locals.values.exists(_.ty.isImprec)

    /** getter */
    def get(x: Var): AbsValue = x match
      case x: Global => base.getOrElse(x, AbsValue.Bot)
      case x: Local  => locals.getOrElse(x, AbsValue.Bot)

    // an unconstrained variadic argument is not in the environment
    def get(sym: Sym): ValueTy =
      val default = if (variadicIdxOf(sym).isDefined) ESValueT else BotT
      symEnv.getOrElse(sym, default)

    def get(sty: SymTy): AbsValue = AbsValue(sty)

    def getTy(sty: SymTy): ValueTy = sty.ty

    def getTy(base: Base): ValueTy = base match
      case l: Local => get(l).ty
      case s: Sym   => get(s)

    def getConstr(sym: Sym): ValueTy = constr.get(sym) && get(sym)

    def constrForSyms: Map[Sym, ValueTy] =
      (for (sym <- symEnv.keySet.toList) yield sym -> getConstr(sym)).toMap

    /** getter */
    def get(base: AbsValue, field: AbsValue)(using AbsState): AbsValue = {
      import SymTy.*
      val guard = lookupGuard(base.guard, field)
      (base.symty, field.ty.getSingle) match
        case (ref: SymRef, One(Str(f))) =>
          AbsValue(SField(ref, STy(StrT(f))), guard)
        case (SNormal(sty), One(Str("Value"))) =>
          AbsValue(sty, guard)
        case (SArgs, One(Math(k))) if k.isValidInt && k.toInt >= 0 =>
          AbsValue(SVariadicIdx(k.toInt), guard)
        case _ =>
          AbsValue(STy(get(base.ty, field.ty)), guard)
    }
    def get(baseTy: ValueTy, fieldTy: ValueTy)(using AbsState): ValueTy =
      lookupAst(baseTy.ast, fieldTy) ||
      lookupStr(baseTy.str, fieldTy) ||
      lookupList(baseTy.list, fieldTy) ||
      lookupRecord(baseTy.record, fieldTy) ||
      lookupMap(baseTy.map, fieldTy)

    /** property getter */
    def getProp(base: AbsValue, prop: Property)(using AbsState): AbsValue = {
      import SymTy.*
      base.symty match
        case ref: SymRef => AbsValue(SProp(ref, prop))
        case _           => AbsValue(STy(base.ty.record(prop).getTy))
    }

    /** function call return getter */
    def getCall(base: AbsValue)(using AbsState): AbsValue = {
      import SymTy.*
      base.symty match
        case ref: SymRef => AbsValue(SCall(ref))
        case _           => AbsValue(STy(base.ty.record.call.getTy))
    }

    /** construct return getter */
    def getConstruct(base: AbsValue)(using AbsState): AbsValue = {
      import SymTy.*
      base.symty match
        case ref: SymRef => AbsValue(SConstruct(ref))
        case _           => AbsValue(STy(base.ty.record.construct.getTy))
    }

    // AST lookup
    private def lookupAst(ast: AstTy, field: ValueTy): ValueTy =
      import AstTy.*
      ast match
        case AstTy.Bot => BotT
        case Detail(name, idx) =>
          lookupAstIdxField(name, idx)(field) ||
          lookupAstStrField(field)
        case Simple(names) =>
          if (!field.math.isBottom) AstT // TODO more precise
          else lookupAstStrField(field)
        case _ => AstT

    // lookup index fields of ASTs
    private def lookupAstIdxField(
      name: String,
      idx: Int,
    )(field: ValueTy): ValueTy = field.math.getSingle match
      case Zero => BotT
      case One(k) =>
        (for {
          prod <- cfg.grammar.nameMap.get(name)
          rhs <- prod.rhsVec.lift(idx)
          nt <- rhs.nts.lift(k.toInt)
        } yield AstT(nt.name)).getOrElse(BotT)
      case Many => AstT

    // lookup string fields of ASTs
    private def lookupAstStrField(field: ValueTy): ValueTy =
      val nameMap = cfg.grammar.nameMap
      field.str.getSingle match
        case Zero                               => BotT
        case One(name) if nameMap contains name => AstT(name)
        case _ => AstT // TODO warning(s"invalid access: $name of $ast")

    // string lookup
    private def lookupStr(str: Flat[String], field: ValueTy): ValueTy =
      if (str.isBottom) BotT
      else {
        var res = BotT
        if (field.str.contains("length")) res ||= NonNegIntT
        if (!field.math.isBottom) res ||= CodeUnitT
        res
      }

    // record lookup
    private def lookupRecord(record: RecordTy, field: ValueTy): ValueTy =
      val str = field.str
      var res = BotT
      def add(fieldStr: String): Unit = res ||= record(fieldStr).value
      if (!record.isBottom) for (fieldStr <- str) add(fieldStr)
      res

    // list lookup
    private def lookupList(list: ListTy, field: ValueTy): ValueTy = list match
      case ListTy.Top        => AnyT
      case ListTy.Elem(elem) => elem
      case ListTy.Bot        => BotT

    // symbol lookup
    private def lookupSymbol(symbol: Boolean, field: ValueTy): ValueTy =
      if (symbol && field.str.contains("Description")) StrT
      else BotT

    // map lookup
    private def lookupMap(map: MapTy, field: ValueTy): ValueTy = map match
      case MapTy.Top              => AnyT
      case MapTy.Elem(key, value) => value
      case MapTy.Bot              => BotT

    // guard lookup
    private def lookupGuard(
      guard: TypeGuard,
      field: AbsValue,
    )(using AbsState): TypeGuard = {
      import TargetType.*
      field.ty.str.getSingle match
        case One("Value") =>
          TypeGuard(guard.map.collect {
            case (dty, map) if dty.ty == NormalT(TrueT) =>
              TargetType(TrueT) -> map
            case (dty, map) if dty.ty == NormalT(FalseT) =>
              TargetType(FalseT) -> map
          })
        case _ => TypeGuard.Empty
    }

    /** define variables */
    def define(x: Var, value: AbsValue): AbsState = x match
      case x: Local  => this.update(x, value, refine = false)
      case x: Global => raise("do not support defining global variables")

    /** identifier setter */
    def update(x: Var, value: AbsValue, refine: Boolean): AbsState = x match
      case x: Local =>
        val newSt = if (refine) this else this.kill(Set(x), update = true)
        val newV =
          if (!refine) value.kill(Set(x), update = true)
          else if (value.hasLocalBase(x)) value.kill(Set(x), update = false)
          else value
        newSt.copy(locals = newSt.locals + (x -> newV), constr = newSt.constr)
      case x: Global => this

    /** type check */
    def typeCheck(value: AbsValue, givenTy: ValueTy): ValueTy =
      val ty = value.ty
      if (ty <= givenTy) TrueT
      else if ((ty && givenTy).isBottom) FalseT
      else BoolT

    /** variable existence check */
    def exists(ref: Ref): AbsValue = ref match
      case x: Local if !locals.contains(x) => AbsValue(FalseT)
      case _                               => AbsValue.BoolTop

    /** expand a field of a record object */
    def expand(base: AbsValue, field: AbsValue): AbsState = this

    /** delete a key from an map object */
    def delete(base: AbsValue, field: AbsValue): AbsState = this

    /** push a value to a list */
    def push(list: AbsValue, value: AbsValue, front: Boolean): AbsState = this

    /** pop a value from a list */
    def pop(list: AbsValue, front: Boolean): (AbsValue, AbsState) =
      (AbsValue(list.ty.list.elem), this)

    /** copy object */
    def copy(from: AbsValue): (AbsValue, AbsState) = (from, this)

    /** get keys of a record/map object as a list */
    def keys(
      base: AbsValue,
      intSorted: Boolean,
    ): (AbsValue, AbsState) =
      val ty = base.ty
      var elemTy = BotT
      if (!ty.record.isBottom) elemTy ||= ty.record.getKey
      if (!ty.map.isBottom) elemTy ||= ty.map.getKey
      if (elemTy.isBottom) (AbsValue.Bot, Bot)
      else (AbsValue(ListT(elemTy)), this)

    /** allocate a record object */
    def allocRecord(
      tname: String,
      pairs: Iterable[(String, AbsValue)] = Nil,
    ): (AbsValue, AbsState) =
      (AbsValue(RecordT(tname, pairs.map(_ -> _.ty).toMap)), this)

    /** allocate a map object */
    def allocMap(
      pairs: Iterable[(AbsValue, AbsValue)] = Nil,
    ): (AbsValue, AbsState) =
      val (keys, values) = pairs.unzip
      val key = keys.foldLeft(BotT)(_ || _.ty)
      val value = values.foldLeft(BotT)(_ || _.ty)
      (AbsValue(MapT(key, value)), this)

    /** allocate a list object */
    def allocList(
      vs: Iterable[AbsValue] = Nil,
    ): (AbsValue, AbsState) =
      (AbsValue(ListT(vs.foldLeft(BotT)(_ || _.ty))), this)
  }
  object AbsState extends DomainLike[AbsState] {

    /** top element */
    lazy val Top: AbsState = exploded("top abstract state")

    /** bottom element */
    lazy val Bot: AbsState =
      AbsState(false, Map(), Map(), TypeConstr.Top)

    /** empty element */
    lazy val Empty: AbsState =
      AbsState(true, Map(), Map(), TypeConstr.Top)

    /** appender */
    given rule: Rule[AbsState] = mkRule(true)

    val constrMapRule: Rule[Map[Sym, ValueTy]] = (app, map) => {
      given Rule[Sym] = (app, sym) => app >> SymTy.SSym(sym)
      sortedMapRule[Sym, ValueTy]("", "", ": ")(app, map)
    }

    // appender generator
    private def mkRule(detail: Boolean): Rule[AbsState] = (app, elem) =>
      import SymTy.given
      if (!elem.isBottom) {
        val AbsState(reachable, locals, symEnv, constr) = elem
        given localsRule: Rule[Map[Local, AbsValue]] = sortedMapRule(sep = ": ")
        given symEnvRule: Rule[Map[Sym, ValueTy]] = sortedMapRule(sep = ": ")
        if (locals.nonEmpty) app >> locals
        if (symEnv.nonEmpty) app >> symEnv
        app >> constr
      } else app >> "⊥"
  }
}
