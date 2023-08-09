package esmeta.analyzer

import esmeta.cfg.*
import esmeta.util.BaseUtils.*

/** static analyzer */
abstract class Analyzer(val cfg: CFG) {

  /** specific shapes of abstract semantics */
  type Semantics <: AbsSemantics

  /** specific abstract transfer function as transfer function */
  type Transfer <: AbsTransfer

  /** transfer function */
  val transfer: Transfer

  /** perform analysis for a given initial abstract semantics */
  def apply(
    init: => Semantics,
    postProcess: Semantics => Semantics = identity,
  ): Semantics = withAnalyzer(this) {
    val sem = init
    withSem(sem) {
      transfer.fixpoint
      postProcess(sem)
    }
  }
}
