package esmeta.analyzer

import esmeta.cfg.*

/** static analyzer */
abstract class Analyzer(val cfg: CFG) {

  /** specific shapes of abstract semantics as results */
  type Result <: AbsSemantics

  /** specific abstract transfer function as transfer function */
  type Transfer <: AbsTransfer

  /** transfer function */
  val transfer: Transfer

  /** perform analysis for a given initial abstract semantics */
  def apply(
    init: => Result,
    postProcess: Result => Result = identity,
  ): Result = withAnalyzer(this) {
    val sem = init
    withSem(sem) {
      transfer.fixpoint
      postProcess(sem)
    }
  }
}
