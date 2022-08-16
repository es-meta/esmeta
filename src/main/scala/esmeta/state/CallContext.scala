package esmeta.state

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}

/** IR calling contexts */
case class CallContext(retId: Id, context: Context) extends StateElem {

  /** function name * */
  def name: String = context.func.irFunc.name

  /** copy contexts */
  def copied: CallContext = copy(context = context.copied)
}
