package esmeta.state

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}

/** IR calling contexts */
case class CallContext(context: Context, retId: Local) extends StateElem {

  /** function name * */
  def name: String = context.func.irFunc.name

  /** copy contexts */
  def copied: CallContext = copy(context = context.copied)
}
