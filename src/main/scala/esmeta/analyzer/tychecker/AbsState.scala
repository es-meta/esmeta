package esmeta.analyzer.tychecker
import esmeta.ir.*
import esmeta.ty.{*, given}
import esmeta.ty.util.Stringifier.{*, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** abstract states */
trait AbsStateDecl { self: TyChecker =>
  case class AbsState(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    symEnv: Map[Sym, ValueTy],
    pred: SymPred,
  ) extends AbsStateLike {
    import AbsState.*

    given AbsState = this

    /** bottom check */
    def isBottom: Boolean = !reachable

    /** bottom inclusion check */
    def hasBottom: Boolean = locals.values.exists(_.isBottom)

    /** partial order */
    def ⊑(that: AbsState): Boolean = (this, that) match
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (
            AbsState(_, llocals, lsymEnv, lpred),
            AbsState(_, rlocals, rsymEnv, rpred),
          ) =>
        val SymPred(lmap, lexpr) = lpred
        val SymPred(rmap, rexpr) = rpred
        llocals.forall { (x, v) => rlocals.get(x).fold(false)(v ⊑ _) } &&
        lsymEnv.forall { (sym, ty) => rsymEnv.get(sym).fold(false)(ty ⊑ _) } &&
        rmap.forall { (r, rty) => lmap.get(r).fold(false)(_ <= rty) } &&
        rexpr.forall { r => lexpr.fold(false)(_ == r) }

    /** not partial order */
    def !⊑(that: AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (l, r) =>
        var killed = Set[Sym]()
        def handleKilled(v: AbsValue)(using AbsState): AbsValue =
          if (killed.exists(v.has)) AbsValue(v.ty, Many, v.guard)
          else v
        val newSymEnv = (for {
          sym <- (l.symEnv.keySet ++ r.symEnv.keySet).toList
          lty = l.getTy(sym)
          rty = r.getTy(sym)
          _ = if (lty != rty) killed += sym
          ty = lty || rty
        } yield sym -> ty).toMap
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = handleKilled(l.get(x))(using l) ⊔ handleKilled(r.get(x))(using r)
        } yield x -> v).toMap
        val newPred = l.pred || r.pred
        AbsState(true, newLocals, newSymEnv, newPred)

    /** meet operator */
    def ⊓(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (l, r) =>
        val newLocals = (for {
          x <- (l.locals.keySet intersect r.locals.keySet).toList
          v = this.get(x) ⊓ that.get(x)
        } yield x -> v).toMap
        val newSymEnv = (for {
          sym <- (l.symEnv.keySet intersect r.symEnv.keySet).toList
          ty = l.getTy(sym) ⊓ r.getTy(sym)
        } yield sym -> ty).toMap
        val newPred = l.pred && r.pred
        AbsState(true, newLocals, newSymEnv, newPred)

    /** has imprecise elements */
    def hasImprec: Boolean = locals.values.exists(_.ty.isImprec)

    /** getter */
    def get(x: Var): AbsValue = x match
      case x: Global => base.getOrElse(x, AbsValue.Bot)
      case x: Local  => locals.getOrElse(x, AbsValue.Bot)

    /** getter for symbolic expressions */
    def getTy(expr: SymExpr): ValueTy = {
      import SymExpr.*
      expr match
        case SEBool(b)                  => BoolT(b)
        case SEStr(s)                   => StrT(s)
        case SERef(ref)                 => getTy(ref)
        case SETypeCheck(base, ty)      => BoolT
        case SEBinary(bop, left, right) => ???
        case SEUnary(uop, expr)         => ???
    }

    /** getter for symbolic references */
    def getTy(ref: SymRef): ValueTy = {
      import SymRef.*
      ref match
        case SBase(x)            => getTy(x)
        case SField(base, field) => get(getTy(base), getTy(field))
    }

    /** getter for symbolic bases */
    def getTy(x: SymBase): ValueTy = x match
      case x: Sym   => symEnv.getOrElse(x, BotT)
      case x: Local => get(x).ty

    /** getter */
    def get(base: AbsValue, field: AbsValue)(using AbsState): AbsValue = {
      import SymExpr.*, SymRef.*
      val guard = lookupGuard(base.guard, field)
      (base.getSymExpr, field.ty.getSingle) match
        case (Some(SERef(ref)), One(Str(f))) =>
          AbsValue(BotT, One(SERef(SField(ref, SEStr(f)))), guard)
        case _ =>
          AbsValue(get(base.ty, field.ty), Zero, guard)
    }
    def get(baseTy: ValueTy, fieldTy: ValueTy)(using AbsState): ValueTy =
      lookupAst(baseTy.ast, fieldTy) ||
      lookupStr(baseTy.str, fieldTy) ||
      lookupList(baseTy.list, fieldTy) ||
      lookupRecord(baseTy.record, fieldTy) ||
      lookupMap(baseTy.map, fieldTy)
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
          rhs <- prod.rhsList.lift(idx)
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
    private def lookupStr(str: BSet[String], field: ValueTy): ValueTy =
      if (str.isBottom) BotT
      else {
        var res = BotT
        if (field.str contains "length") res ||= NonNegIntT
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
    private def lookupList(list: ListTy, field: ValueTy): ValueTy =
      var res = BotT
      val str = field.str
      val math = field.math
      list match
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
      base: TypeGuard,
      field: AbsValue,
    )(using AbsState): TypeGuard = {
      import RefinementKind.*
      field.ty.str.getSingle match
        case One("Value") =>
          base.collect {
            case (NormalTrue, map)  => True -> map
            case (NormalFalse, map) => False -> map
          }
        case _ => Map()
    }

    /** define variables */
    def define(x: Var, value: AbsValue): AbsState = x match
      case x: Local  => this.copy(locals = locals + (x -> value))
      case x: Global => error("do not support defining global variables")

    /** identifier setter */
    def update(x: Var, value: AbsValue, refine: Boolean): AbsState = x match
      case x: Local =>
        val newSt = copy(locals = locals + (x -> value))
        if (refine) newSt
        else newSt.kill(x)
      case x: Global => this

    /** kill a local variable */
    def kill(x: Local): AbsState =
      val newLocals = locals.map { (y, v) =>
        val newGuard = for {
          (kind, pred) <- v.guard
          newPred = pred.kill(x)
        } yield kind -> newPred
        y -> v.copy(guard = newGuard)
      }
      val newPred = pred.kill(x)
      AbsState(reachable, newLocals, symEnv, newPred)

    /** type check */
    def tycheck(value: AbsValue, ty: ValueTy): ValueTy =
      if (value.ty <= ty) TrueT else BoolT

    /** variable existence check */
    def exists(ref: Ref): AbsValue = AbsValue.BoolTop

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
    lazy val Bot: AbsState = AbsState(false, Map(), Map(), SymPred())

    /** empty element */
    lazy val Empty: AbsState = AbsState(true, Map(), Map(), SymPred())

    /** appender */
    given rule: Rule[AbsState] = mkRule(true)

    // appender generator
    private def mkRule(detail: Boolean): Rule[AbsState] = (app, elem) =>
      if (!elem.isBottom) {
        val AbsState(reachable, locals, symEnv, pred) = elem
        given localsRule: Rule[Map[Local, AbsValue]] = sortedMapRule(sep = ": ")
        given symEnvRule: Rule[Map[Sym, ValueTy]] = sortedMapRule(sep = ": ")
        given predRule: Rule[Map[SymBase, ValueTy]] =
          sortedMapRule(sep = " <: ")
        app >> locals
        if (symEnv.nonEmpty) app >> symEnv
        app >> pred
        app
      } else app >> "⊥"
  }
}
