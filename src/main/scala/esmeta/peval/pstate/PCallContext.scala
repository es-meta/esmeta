package esmeta.peval.pstate

import esmeta.ir.{Local}
import esmeta.state.{StateElem}
import esmeta.state.CallContext

/** IR CallContext for partial Evaluation. similar to CallContext.scala */
case class PCallContext(context: PContext, retId: Local) extends StateElem {

  /** function name * */
  def name: String = context.func.name

  /** copy contexts */
  def copied: PCallContext = copy(context = context.copied)
}

object PCallContext {
  def fromCallContext(cc: CallContext): PCallContext = PCallContext(
    context = PContext.fromContext(cc.context),
    retId = cc.retId,
  )
}
