package esmeta.ai

import esmeta.ai.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*

/** meta-level static analyzer for ECMAScript */
class ESAnalyzer(sourceText: String) extends Analyzer {

  /** perform analysis */
  def apply(cfg: CFG): AbsSemantics = getSemantics.fixpoint

  /** initial control point */
  lazy val initCp =
    val runJobs = Config.cfg.fnameMap("RunJobs")
    val entry = runJobs.entry.get
    NodePoint(runJobs, entry, View())

  /** abstract semantics */
  def getSemantics: AbsSemantics = AbsSemantics(
    npMap = Map(
      initCp -> AbsState.Empty.defineGlobal(
        Global(builtin.SOURCE_TEXT) -> AbsValue(sourceText),
      ),
    ),
  )
}
