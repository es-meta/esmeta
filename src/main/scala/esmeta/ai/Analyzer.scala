package esmeta.ai

// TODO import esmeta.ai.domain.*
import esmeta.ai.sensitivity.*
import esmeta.cfg.*

/** specification analyzer in control-flow graph of ECMA-262 */
trait Analyzer {
  val sensitivity: Sensitivity // analysis sensitivity
  // TODO val stateDomain: StateDomain // abstract state domain
  // TODO val valueDomain: ValueDomain // abstract value domain
  // TODO val transfer: AbsTransfer // abstract transfer function

  /** perform analysis */
  // TODO def apply(cfg: CFG): AbsSemantics
}
