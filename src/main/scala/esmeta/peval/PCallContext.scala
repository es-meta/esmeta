package esmeta.peval

import esmeta.ir.{Local}
import esmeta.state.{StateElem}

/** IR CallContext for partial Evaluation. similar to CallContext.scala */
case class PCallContext(context: PContext, retId: Local) extends StateElem {

  /** function name * */
  def name: String = context.func.name

  /** copy contexts */
  def copied: PCallContext = copy(context = context.copied)
}
