package esmeta.analyzer.domain

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.ir.{Id, Local, Global, IRElem, Type => IrType}
import esmeta.js
import esmeta.js.builtin.*
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.StateMonad
import scala.annotation.targetName // TODO remove this

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
    // TODO
    // HOST_DEFINED -> AbsValue.undef,
    // INTRINSICS -> NamedAddr(INTRINSICS),
    // GLOBAL -> NamedAddr(GLOBAL),
    // SYMBOL -> NamedAddr(SYMBOL),
    // REALM -> NamedAddr(REALM),
    // JOB_QUEUE -> NamedAddr(JOB_QUEUE),
    // SYMBOL_REGISTRY -> NamedAddr(SYMBOL_REGISTRY),
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
          case ast: AstTBase   => lookupAst(ast, p)
          case StrT            => lookupStr(StrT, p)
          case str: StrSingleT => lookupStr(str, p)
          case list: ListT     => lookupList(list, p)
          case obj: NameT      => lookupNamedRec(obj, p)
          case _               => ??? // TODO
      } yield v
      AbsValue(vset.toList: _*)
    def lookupComp(comp: CompType, prop: Type): Set[Type] = ???
    def lookupAst(ast: AstTBase, prop: Type): Set[Type] = {
      var tySet: Set[Type] = Set()
      prop match
        // access to child
        case MathSingleT(n) if n.isValidInt =>
          val propIdx = n.toInt
          def addNts(nts: List[Option[String]]): Unit =
            if (propIdx >= nts.size) ??? // TODO warning
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
        case StrSingleT("parent") => ??? // TODO
        // access to child
        case StrSingleT(str) =>
          for {
            rhs <- cfg.grammar.nameMap(ast.name).rhsList
            nt <- rhs.nts if nt.name == str
          } tySet += AstT(nt.name)
        case _ => ??? // TODO warning invalid access
      tySet
    }
    def lookupStr(str: StrT.type, prop: Type): Set[Type] = ???
    def lookupStr(str: StrSingleT, prop: Type): Set[Type] = ???
    def lookupList(list: ListT, prop: Type): Set[Type] =
      var tySet: Set[Type] = Set()
      prop match
        // length
        case StrSingleT("length") => tySet += MathT
        // element
        case MathT | (_: MathSingleT) =>
          tySet += list.elem
        case _ => ??? // TODO warning invalid access
      tySet
    def lookupNamedRec(obj: NameT, prop: Type): Set[Type] =
      prop match
        case StrSingleT(propStr) =>
          cfg.typeModel.getProp(obj.name, propStr)
        case _ => ??? // TODO
    def apply(loc: Loc): AbsObj = ???
    def lookupGlobal(x: Global): AbsValue =
      baseGlobals.getOrElse(x, AbsValue.Bot)

    /** define global variables */
    def defineGlobal(pairs: (Global, AbsValue)*): Elem = ???

    /** define local variables */
    def defineLocal(pairs: (Local, AbsValue)*): Elem =
      bottomCheck(pairs.unzip._2) { copy(locals = locals ++ pairs) }

    /** setters */
    def update(x: Id, value: AbsValue): Elem = x match
      case x: Local  => defineLocal(x -> value)
      case Global(x) => ???
    def update(aloc: AbsValue, prop: AbsValue, value: AbsValue): Elem = ???

    /** default value for bottom check */
    given bottomValue: AbsValue = AbsValue.Bot

    /** object operators */
    def delete(refV: AbsRefValue): Elem = ???
    def append(loc: AbsLoc, value: AbsValue): Elem = ???
    def prepend(loc: AbsLoc, value: AbsValue): Elem = ???
    def remove(loc: AbsLoc, value: AbsValue): Elem = ???
    def pop(loc: AbsLoc, front: Boolean): (AbsValue, Elem) = ???
    def copyObj(from: AbsLoc, to: AllocSite): Elem = ???
    def keys(loc: AbsLoc, intSorted: Boolean, to: AllocSite): Elem = ???
    def setType(loc: AbsLoc, tname: String): Elem = ???
    def allocMap(
      tname: String,
      pairs: List[(AbsValue, AbsValue)],
      to: AllocSite,
    ): (AbsValue, Elem) =
      bottomCheck(pairs.flatMap { case (k, v) => List(k, v) }) {
        (???, this)
      }
    def allocList(list: List[AbsValue], to: AllocSite): (AbsValue, Elem) =
      bottomCheck(list) {
        val elemTy =
          list.map(_.removeAbsent.upcast).foldLeft(AbsValue.Bot: AbsValue) {
            case (merged, elem) => merged ⊔ elem
          }
        val listTy = elemTy.set.size match
          case 0 => NilT
          case 1 => ListT(elemTy.set.head.toPureType)
          case _ => ??? // TODO
        (AbsValue(listTy), this)
      }
    def allocSymbol(desc: AbsValue, to: AllocSite): (AbsValue, Elem) =
      bottomCheck(desc) {
        // check desc is string or undefined
        desc.assert(ty => ty.isStr || ty.isUndef)
        (AbsValue(SymbolT), this)
      }
    def contains(
      list: AbsValue,
      value: AbsValue,
      field: Option[(IrType, String)],
    ): AbsValue = ???

    /** singleton location checks */
    def isSingle(loc: Loc): Boolean = ???

    /** find merged parts */
    def findMerged: Unit = ???

    /** handle calls */
    def doCall: Elem = this
    def doProcStart(fixed: Set[Loc]): Elem = ???

    /** handle returns (this: return states / to: caller states) */
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = Elem(
      reachable = true,
      locals = to.locals ++ defs,
    )
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem = ???
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = ???
    def garbageCollected: Elem = ???

    /** get reachable locations */
    def reachableLocs: Set[Loc] = Set() // XXX not used

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
    def getString(value: AbsValue): String = ???
  }
}
