package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*

/** meta-level static analyzer for ECMAScript */
class ESAnalyzer(
  val cfg: CFG,
  override val useRepl: Boolean = false,
) extends Analyzer {

  /** perform type analysis with the given control flow graph */
  def apply(sourceText: String): Semantics =
    AbsState.setBase(new Initialize(cfg))
    sem.reset(AbsSemantics(initNpMap(sourceText)))
    transfer.fixpoint
    sem

  /** abstract semantics */
  lazy val sem: AbsSemantics = new AbsSemantics
  type Semantics = AbsSemantics

  /** transfer function */
  lazy val transfer: Transfer = new Transfer
  class Transfer extends AbsTransfer

  /** throw exception for not yet compiled expressions */
  override val yetThrow: Boolean = true

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
