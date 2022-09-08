package esmeta.analyzer

import esmeta.ANALYZE_LOG_DIR
import esmeta.analyzer.domain.*
import esmeta.ir.Param
import esmeta.cfg.*

/** specification type analyzer for ECMA-262 */
private class TypeAnalyzer(cfg: CFG, targets: List[Func]) {

  // initilize CFG for analysis
  lazy val result: AbsSemantics = withCFG(cfg) {
    withSem(TypeSemantics(initNpMap)) {
      withLog(s"$ANALYZE_LOG_DIR/type-check") {
        sem.fixpoint
      }
    }
  }

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
