package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*

/** meta-level static analyzer for ECMAScript */
class ESAnalyzer(cfg: CFG) extends Analyzer(cfg) {

  /** default abstract semantics */
  type Semantics = AbsSemantics

  /** default abstract transfer function as transfer */
  trait Transfer extends AbsTransfer

  /** transfer function */
  object transfer extends Transfer

  def addMismatch(mismatch: TypeMismatch): Unit = ()

  /** perform analysis for a given ECMAScript code */
  def apply(sourceText: String): AbsSemantics =
    apply(AbsSemantics(initNpMap(sourceText)))

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  /** initial control point */
  private lazy val initCp =
    val runJobs = cfg.fnameMap("RunJobs")
    val entry = runJobs.entry
    NodePoint(runJobs, entry, View())

  /** get initial abstract states in each node point */
  private def initNpMap(
    sourceText: String,
  ): Map[NodePoint[Node], AbsState] = Map(
    initCp -> AbsState.Empty.defineGlobal(
      Global(builtin.SOURCE_TEXT) -> AbsValue(sourceText),
    ),
  )
}
object ESAnalyzer:
  // throw exceptions when touching not yet supported instructions
  YET_THROW = true
