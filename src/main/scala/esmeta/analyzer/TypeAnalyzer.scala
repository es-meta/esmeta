package esmeta.analyzer

import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.ir.{Name, Param, Type}
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ty.*
import esmeta.util.*

/** specification type analyzer for ECMA-262 */
class TypeAnalyzer(cfg: CFG, targets: List[Func]) {

  // set CFG for analysis
  setCFG(cfg)

  // analysis result
  lazy val result = new AbsSemantics(initNpMap) {

    /** abstract transfer function */
    override val transfer: AbsTransfer = TypeTransfer(this)

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
            case ty: ValueTy =>
              val newArg = AbsValue(ty)
              if (arg !⊑ newArg)
                logger.warn(s"invalid input: $arg !⊑ $newArg")
              newArg
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
    case (st, Param(x, ty, opt)) =>
      var v = AbsValue(ty.ty)
      if (opt) v ⊔= AbsValue.absentTop
      st.update(x, v)
  }
}
object TypeAnalyzer:
  // set type domains
  setDomain(
    stateDomain = state.TypeDomain,
    valueDomain = value.TypeDomain,
    retDomain = ret.TypeDomain,
  )

  // no sensitivity
  IR_SENS = false

  // use type refinement
  USE_REFINE = true

  /** perform analysis for a given ECMAScript code */
  def apply(
    cfg: CFG,
    targets: List[Func],
  ): AbsSemantics = new TypeAnalyzer(cfg, targets).result
