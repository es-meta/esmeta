package esmeta.ai

// TODO import esmeta.ai.domain.*
import esmeta.cfg.*

/** specification analyzer in control-flow graph of ECMA-262 */
trait Analyzer {

  /** perform analysis */
  def apply(cfg: CFG): AbsSemantics
}
