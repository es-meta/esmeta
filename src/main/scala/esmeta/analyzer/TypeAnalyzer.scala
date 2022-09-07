package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.ir.{Name, Param, Type}
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*

/** specification type analyzer for ECMA-262 */
class TypeAnalyzer(cfg: CFG, targets: List[Func]) {

  // initilize CFG for analysis
  initCFG(cfg)

  // analysis result
  lazy val result = new AbsSemantics(initNpMap) {

    /** handle calls */
    override def doCall(
      callerNp: NodePoint[Call],
      callerSt: AbsState,
      calleeFunc: Func,
      args: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
    ): Unit =
      // get parameter types
      val newArgs = (args zip calleeFunc.params).map {
        case (arg, param) =>
          param.ty.ty match
            case _: UnknownTy => arg
            case ty: ValueTy  => AbsValue(ty)
      }
      val NodePoint(callerFunc, call, view) = callerNp
      calleeFunc.retTy.ty match
        case retTy: ValueTy if calleeFunc.isParamTysDefined =>
          for {
            nextNode <- call.next
            nextNp = NodePoint(callerFunc, nextNode, View())
            retV = AbsValue(retTy)
            newSt = callerSt.defineLocal(call.lhs -> retV)
          } this += nextNp -> newSt
        case _ =>
          super.doCall(callerNp, callerSt, calleeFunc, newArgs, captured)

    /** conversion to string */
    override def toString: String =
      (new Appender >> analyzedFuncs.toList.sortBy(_.name)).toString

    /** conversion helper to string */
    given getRule: Rule[Iterable[Func]] = (app, funcs) =>
      import TyStringifier.given
      given Rule[Iterable[(String, ValueTy)]] = iterableRule("(", ", ", ")")
      app >> "-" * 80
      for (func <- funcs) {
        val rp = ReturnPoint(func, View())
        rpMap.get(rp) match
          case None =>
            app :> "analysis of " >> func.name >> " does not terminate."
          case Some(ret) =>
            app :> "   " >> func.headString
            val fname = func.name
            val entryNp = NodePoint(func, func.entry, View())
            val st = this(entryNp)
            val newParams = for {
              p <- func.params
            } yield {
              p.lhs.name -> st.get(p.lhs, entryNp).ty
            }
            app :> "-> " >> "def " >> fname >> newParams
            app >> ": " >> ret.value.ty
        app :> "-" * 80
      }
      app

    given paramRule: Rule[(String, ValueTy)] = (app, pair) =>
      import TyStringifier.given
      val (param, ty) = pair
      app >> param
      if (ty.absent) app >> "?"
      app >> ": " >> ty -- AbsentT

    /** update return points */
    override def doReturn(rp: ReturnPoint, origRet: AbsRet): Unit =
      val ReturnPoint(func, view) = rp
      val newRet = rp.func.retTy.ty match
        case _: UnknownTy => origRet
        case ty: ValueTy  => AbsRet(AbsValue(ty))
      super.doReturn(rp, newRet)
  }.fixpoint

  // all entry node points
  lazy val nps: List[NodePoint[Node]] = for {
    func <- targets
    entry = func.entry
    view = getView(func)
  } yield NodePoint(func, entry, view)

  // get initial abstract states in each node point
  lazy val initNpMap: Map[NodePoint[Node], AbsState] = (for {
    np @ NodePoint(func, _, _) <- nps
    st = getState(func)
  } yield np -> st).toMap

  // get view from a function
  def getView(func: Func): View = View()

  // get return point
  def getState(func: Func): AbsState = func.params.foldLeft(AbsState.Empty) {
    case (st, Param(x, ty, opt, _)) =>
      var v = AbsValue(ty.ty)
      if (opt) v ⊔= AbsValue.absentTop
      st.update(x, v)
  }
}
object TypeAnalyzer:
  // set type domains
  initDomain(
    stateDomain = state.TypeDomain,
    valueDomain = value.TypeDomain,
    retDomain = ret.TypeDomain,
  )

  // no sensitivity
  IR_SENS = false

  // use type refinement
  USE_REFINE = true

  /** perform type analysis for given target functions */
  def apply(
    cfg: CFG,
    targets: List[Func],
  ): AbsSemantics = new TypeAnalyzer(cfg, targets).result
