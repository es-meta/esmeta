package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*

/** meta-level static analyzer for ECMAScript */
class ESAnalyzer(cfg: CFG) {

  /** perform analysis for a given ECMAScript code */
  def apply(sourceText: String): AbsSemantics = withCFG(cfg) {
    withSem(AbsSemantics(initNpMap(sourceText))) {
      sem.fixpoint
    }
  }

  /** initial control point */
  lazy val initCp =
    val runJobs = cfg.fnameMap("RunJobs")
    val entry = runJobs.entry
    NodePoint(runJobs, entry, View())

  // get initial abstract states in each node point
  def initNpMap(sourceText: String): Map[NodePoint[Node], AbsState] = Map(
    initCp -> AbsState.Empty.defineGlobal(
      Global(builtin.SOURCE_TEXT) -> AbsValue(sourceText),
    ),
  )
}
object ESAnalyzer:
  // throw exceptions when touching not yet supported instructions
  YET_THROW = true
