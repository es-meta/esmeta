package esmeta.analyzer.domain

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.util.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.ir.{Id, Local, Global, IRElem, Type => IrType}
import esmeta.js
import esmeta.js.builtin.*
import esmeta.spec.TypeModel
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.StateMonad

/** abstract states for type analysis */
object TypeStateDomain extends StateDomain {

  // TODO remove unsafe type casting
  given Conversion[AbsValue, TypeDomain.Elem] = _.asInstanceOf[TypeDomain.Elem]
  given Conversion[TypeDomain.Elem, AbsValue] = _.asInstanceOf[AbsValue]

  /** bottom element */
  val Bot: Elem = Elem(false, Map())

  /** empty element */
  val Empty: Elem = Elem(true, Map())

  /** base globals */
  // TODO global modeling
  lazy val baseGlobals: Map[Id, AbsValue] = Map(
    EXECUTION_STACK -> AbsValue(ListT(NameT("ExecutionContext"))),
    HOST_DEFINED -> AbsValue.undef,
    SYMBOL_REGISTRY -> AbsValue(ListT(NameT("GlobalSymbolRegistryRecord"))),
    // TODO
    // INTRINSICS -> NamedAddr(INTRINSICS),
    // GLOBAL -> NamedAddr(GLOBAL),
    // SYMBOL -> NamedAddr(SYMBOL),
    // REALM -> NamedAddr(REALM),
    // JOB_QUEUE -> NamedAddr(JOB_QUEUE),
    UNDEF_TYPE -> AbsValue("Undefined"),
    NULL_TYPE -> AbsValue("Null"),
    BOOL_TYPE -> AbsValue("Boolean"),
    STRING_TYPE -> AbsValue("String"),
    SYMBOL_TYPE -> AbsValue("Symbol"),
    NUMBER_TYPE -> AbsValue("Number"),
    BIGINT_TYPE -> AbsValue("BigInt"),
    OBJECT_TYPE -> AbsValue("Object"),
  ).map { case (k, v) => Global(k) -> v }

  /** appender */
  given rule: Rule[Elem] = (app, elem) => {
    val irStringifier = IRElem.getStringifier(true, false)
    import irStringifier.given
    if (elem.isBottom) app >> "⊥"
    else
      app.wrap {
        app :> "locals: " >> elem.locals >> LINE_SEP
      }
  }

  /** elements */
  case class Elem(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
  ) extends StateElemTrait {

    /** join operator */
    def ⊔(that: Elem): Elem = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (l, r) =>
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = this.lookupLocal(x) ⊔ that.lookupLocal(x)
        } yield x -> v).toMap
        Elem(true, newLocals)

    /** meet operator */
    def ⊓(that: Elem): Elem = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (l, r) =>
        var isBottom = false
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = this.lookupLocal(x) ⊓ that.lookupLocal(x)
          _ = isBottom |= v.isBottom
        } yield x -> v).toMap
        if (isBottom) Bot
        else Elem(true, newLocals)

    /** getters */
    def apply(base: AbsValue, prop: AbsValue): AbsValue =
      val vset = for {
        ty <- base.set
        p <- prop.set
        v <- ty match
          case comp: CompType  => lookupComp(comp, p)
          case AstTopT         => lookupAst(p)
          case ast: AstTBase   => lookupAst(ast, p)
          case StrT            => lookupStr(p)
          case str: StrSingleT => lookupStr(str, p)
          case list: ListT     => lookupList(list, p)
          case NilT            => lookupList(p)
          case obj: NameT      => lookupNamedRec(obj, p)
          case MapT(elemTy)    => Set(elemTy, AbsentT)
          case _ =>
            warning(s"invalid property access: $ty[$p]")
            Set()
      } yield v
      AbsValue(vset.toList: _*)
    private def lookupComp(comp: CompType, prop: Type): Set[Type] =
      import TypeModel.*
      // TODO optimize
      (comp, prop) match
        case (NormalT(t), StrSingleT("Value"))  => Set(t)
        case (NormalT(_), StrSingleT("Type"))   => Set(NORMAL)
        case (NormalT(_), StrSingleT("Target")) => Set(EMPTY)
        case (AbruptT, StrSingleT("Value"))     => Set(ESValueT, EMPTY)
        case (AbruptT, StrSingleT("Type")) =>
          Set(BREAK, CONTINUE, RETURN, THROW)
        case (AbruptT, StrSingleT("Target")) => Set(StrT, EMPTY)
        case _ =>
          warning(s"invalid completion property access: $comp[$prop]")
          Set()
    private def lookupAst(prop: Type): Set[Type] =
      prop match
        // access to child
        case MathSingleT(n) if n.isValidInt => Set(AstTopT)
        // access to parent
        case StrSingleT(_) => Set(AstTopT)
        case _ =>
          warning(s"invalid ast property access: ${AstTopT}[$prop]")
          Set()
    private def lookupAst(ast: AstTBase, prop: Type): Set[Type] = {
      var tySet: Set[Type] = Set()
      prop match
        // access to child
        case MathSingleT(n) if n.isValidInt =>
          val propIdx = n.toInt
          def addNts(nts: List[Option[String]]): Unit =
            if (propIdx >= nts.size)
              warning(s"invalid ast property access: $ast[$propIdx]")
            else
              tySet += nts(propIdx)
                .map(ntName => AstT(ntName))
                .getOrElse(AbsentT)
          ast match
            case AstT(name) =>
              for {
                rhs <- cfg.grammar.nameMap(name).rhsList
                subIdx <- (0 until rhs.countSubs)
              } addNts(rhs.getNts(subIdx))
            case SyntacticT(name, idx, subIdx) =>
              val rhs = cfg.grammar.nameMap(name).rhsList(idx)
              addNts(rhs.getNts(subIdx))
        // access to parent
        case StrSingleT("parent") => tySet += AstTopT
        // access to child
        case StrSingleT(str) =>
          for {
            rhs <- cfg.grammar.nameMap(ast.name).rhsList
            nt <- rhs.nts if nt.name == str
          } tySet += AstT(nt.name)
        case _ => warning(s"invalid ast property access: $ast[$prop]")
      tySet
    }
    private def lookupStr(prop: Type): Set[Type] = prop match
      case StrSingleT("length")   => Set(MathT)
      case MathT | MathSingleT(_) => Set(CodeUnitT)
      case _ =>
        warning(s"invalid string property access: ${StrT}[$prop]")
        Set()
    private def lookupStr(str: StrSingleT, prop: Type): Set[Type] =
      lookupStr(prop) // XXX more precise?
    private def lookupList(list: ListT, prop: Type): Set[Type] =
      var tySet: Set[Type] = Set()
      prop match
        // length
        case StrSingleT("length") => tySet += MathT
        // element
        case MathT | (_: MathSingleT) => tySet += list.elem
        case _ => warning(s"invalid list property access: $list[$prop]")
      tySet
    private def lookupList(prop: Type): Set[Type] =
      prop match
        // length
        case StrSingleT("length") => Set(MathSingleT(0))
        case _ =>
          warning(s"invalid list property access: ${NilT}[$prop]")
          Set()
    private def lookupNamedRec(obj: NameT, prop: Type): Set[Type] =
      prop match
        case StrSingleT(propStr) =>
          cfg.typeModel.getProp(obj.name, propStr)
        case StrT => Set(TopT) // XXX warning imprecision
        case _ =>
          warning(s"invalid record property access: ${obj.name}[$prop]")
          Set()
    def apply(loc: Loc): AbsObj = notSupported(this, "apply")
    def lookupGlobal(x: Global): AbsValue =
      baseGlobals.getOrElse(x, AbsValue.Bot)

    /** define global variables */
    def defineGlobal(pairs: (Global, AbsValue)*): Elem =
      notSupported(this, "defineGlobal")

    /** define local variables */
    def defineLocal(pairs: (Local, AbsValue)*): Elem =
      bottomCheck(pairs.unzip._2) { copy(locals = locals ++ pairs) }

    /** setters */
    def update(x: Id, value: AbsValue): Elem = x match
      case x: Local => defineLocal(x -> value)
      case x: Global =>
        if (value !⊑ baseGlobals(x))
          warning(s"invalid global variable update: $x = $value")
        this
    def update(base: AbsValue, prop: AbsValue, value: AbsValue): Elem =
      val origV = apply(base, prop)
      if (value !⊑ origV)
        // XXX handle ArrayCreate, ...
        warning(s"invalid property update: $base[$prop] = $value")
      this

    /** default value for bottom check */
    given bottomValue: AbsValue = AbsValue.Bot

    /** object operators */
    def delete(refV: AbsRefValue): Elem = this
    def push(list: AbsValue, elem: AbsValue, front: Boolean): Elem =
      bottomCheck(list, elem) {
        for { listTy <- list.set } listTy match
          case NilT => /* do nothing */
          case ListT(elemTy) =>
            if (elem !⊑ AbsValue(elemTy))
              warning(s"invalid element type for push: $listTy <- $elem")
          case _ => warning(s"invalid type for push: $listTy")
        this
      }
    def remove(list: AbsValue, elem: AbsValue): Elem =
      bottomCheck(list, elem) {
        for { listTy <- list.set } listTy match
          case NilT => /* do nothing */
          case ListT(elemTy) =>
            if (elem !⊑ AbsValue(elemTy))
              warning(s"invalid element type for remove: $listTy <- $elem")
          case _ => warning(s"invalid type for remove: $listTy")
        this
      }
    def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) =
      bottomCheck(list) {
        var elemTyOpt: Option[Type] = None
        for { listTy <- list.set } listTy match
          case ListT(elemTy) =>
            elemTyOpt match
              case None      => elemTyOpt = Some(elemTy)
              case Some(ty0) => elemTyOpt = Some(elemTy.lca(ty0))
          case _ => warning(s"invalid type for pop: $listTy")
        elemTyOpt match
          case None         => (AbsValue.Bot, Bot)
          case Some(elemTy) => (AbsValue(elemTy), this)
      }

    def setType(v: AbsValue, tname: String): (AbsValue, Elem) =
      bottomCheck(v) { v.assertNamedRec; (AbsValue(NameT(tname)), this) }
    def copyObj(from: AbsValue, to: AllocSite): (AbsValue, Elem) =
      bottomCheck(from) { from.assertObj; (from, this) }
    def keys(
      v: AbsValue,
      intSorted: Boolean,
      to: AllocSite,
    ): (AbsValue, Elem) =
      bottomCheck(v) {
        var tySet: Set[Type] = Set()
        for { ty <- v.set } ty match
          case NameT(_) => tySet += ListT(StrT)
          case MapT(_)  => tySet += ListT(StrT) // XXX check soundness
          case _        => warning(s"invalid type for keys: $ty")
        (AbsValue(tySet.toList: _*), this)
      }
    def listConcat(ls: List[AbsValue], to: AllocSite): (AbsValue, Elem) =
      bottomCheck(ls) {
        var elemTyOpt: Option[Type] = None
        for {
          l <- ls
          ty <- l.set
        } ty match {
          case ListT(elemTy) =>
            elemTyOpt match
              case None      => elemTyOpt = Some(elemTy)
              case Some(ty0) => elemTyOpt = Some(elemTy.lca(ty0))
          case NilT => /* do nothing */
          case _    => warning(s"invalid type for list concat: $ty")
        }
        (AbsValue(elemTyOpt.map(ListT(_)).getOrElse(NilT)), this)
      }
    def getChildren(
      ast: AbsValue,
      kindOpt: Option[AbsValue],
      to: AllocSite,
    ): (AbsValue, Elem) =
      bottomCheck(ast) {
        ast.assertAst
        (AbsValue(ListT(AstTopT)), this) // XXX check soundness
      }

    def allocMap(
      tname: String,
      pairs: List[(AbsValue, AbsValue)],
      to: AllocSite,
    ): (AbsValue, Elem) =
      bottomCheck(pairs.flatMap { case (k, v) => List(k, v) }) {
        val mapTy =
          if (cfg.typeModel.infos contains tname) {
            // TODO property check
            NameT(tname)
          } else if (tname == "Record") {
            RecordT(
              (for { (k, v) <- pairs } yield k.getSingle match
                  case FlatElem(ASimple(Str(key))) => key -> v.set
                  case _                           => ???, // TODO
              ).toMap,
            )
          } else {
            println(("!!!", tname))
            ??? // TODO
          }
        (AbsValue(mapTy), this)
      }
    def allocList(list: List[AbsValue], to: AllocSite): (AbsValue, Elem) =
      bottomCheck(list) {
        val elemTy =
          list.map(_.removeAbsent.upcast).foldLeft(AbsValue.Bot: AbsValue) {
            case (merged, elem) => merged ⊔ elem
          }
        val listTy = elemTy.set.headOption match
          case None     => NilT
          case Some(ty) => ListT(elemTy.set.tail.foldLeft(ty)(_ lca _))
        (AbsValue(listTy), this)
      }
    def allocSymbol(desc: AbsValue, to: AllocSite): (AbsValue, Elem) =
      bottomCheck(desc) {
        // check desc is string or undefined
        desc.assert(
          ty => ty.isStr || ty.isUndef,
          s"$desc may not be string or undefined",
        )
        (AbsValue(SymbolT), this)
      }
    def contains(
      list: AbsValue,
      value: AbsValue,
      field: Option[(IrType, String)],
    ): AbsValue =
      bottomCheck(list, value) {
        var set: Set[Boolean] = Set()
        for { listTy <- list.set } listTy match
          case ListT(elemTy) =>
            field match
              case Some(_, f) =>
                val origV = apply(AbsValue(elemTy), AbsValue(f))
                if ((value ⊓ origV).isBottom) set += false
                else set = Set(true, false)
              case _ =>
                if ((value ⊓ AbsValue(elemTy)).isBottom) set += false
                else set = Set(true, false)
          case NilT => set += false
          case _    => warning(s"invalid type for contains check: $listTy")
        (
          if (set.isEmpty) AbsValue.Bot
          else if (set.size > 1) AbsValue.bool
          else if (set contains true) AVT
          else AVF,
          this,
        )
      }._1

    /** singleton location checks */
    def isSingle(loc: Loc): Boolean = notSupported(this, "isSingle")

    /** find merged parts */
    def findMerged: Unit = notSupported(this, "findMerged")

    /** handle calls */
    def doCall: Elem = this
    def doProcStart(fixed: Set[Loc]): Elem = notSupported(this, "doProcStart")

    /** handle returns (this: return states / to: caller states) */
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = Elem(
      reachable = true,
      locals = to.locals ++ defs,
    )
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem =
      notSupported(this, "doProcEnd")
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem =
      notSupported(this, "doProcEnd")
    def garbageCollected: Elem =
      notSupported(this, "garbageCollected")

    /** get reachable locations */
    def reachableLocs: Set[Loc] = Set()

    /** copy */
    def copied(
      locals: Map[Local, AbsValue] = Map(),
    ): Elem = copy(locals = locals)

    /** conversion to string */
    def toString(detail: Boolean = false): String = {
      val app = new Appender
      app >> this
      app.toString
    }

    /** get string wth detailed shapes of locations */
    def getString(value: AbsValue): String = value.toString
  }
}
