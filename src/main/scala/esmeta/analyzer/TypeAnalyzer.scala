package esmeta.analyzer

import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.ir.Param
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ty.*
import esmeta.util.*

/** specification type analyzer for ECMA-262 */
class TypeAnalyzer(cfg: CFG) {

  // set CFG for analysis
  setCFG(cfg)

  // analysis result
  lazy val result = new AbsSemantics(initNpMap, initRpMap) {

    /** a worklist of control points */
    override val worklist: Worklist[ControlPoint] =
      QueueWorklist(nps.filter(_.func.params.forall(_.ty.isDefined)))
  }.fixpoint

  // all entry node points
  lazy val nps: List[NodePoint[Node]] = for {
    func <- cfg.funcs
    entry <- func.entry
    view = getView(func)
  } yield NodePoint(func, entry, view)

  // get initial abstract states in each node point
  lazy val initNpMap: Map[NodePoint[Node], AbsState] = (for {
    np @ NodePoint(func, _, _) <- nps
    st = getState(func)
  } yield np -> st).toMap

  // get initial abstract states in each return point
  lazy val initRpMap: Map[ReturnPoint, AbsRet] = (for {
    np @ NodePoint(func, _, _) <- nps
    rp = np.toReturnPoint
    ret = AbsRet(AbsValue(func.retTy.ty))
  } yield rp -> ret).toMap

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

  /** perform analysis for a given ECMAScript code */
  def apply(cfg: CFG): AbsSemantics = new TypeAnalyzer(cfg).result
