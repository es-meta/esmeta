package esmeta.peval.pstate

import esmeta.ir.{Local, Expr, Func => IRFunc}
import esmeta.peval.domain.*
import esmeta.state.{StateElem, Value}
import scala.collection.mutable.{Map => MMap}

/** IR Context for partial Evaluation. similar to state/Context.scala */
case class PContext private (
  val func: IRFunc,
  val locals: MMap[Local, PValue] = MMap(),
) extends StateElem {

  /** copy contexts */
  def copied: PContext = {
    val newContext = copy(locals = MMap.from(locals))
    newContext
  }
}

object PContext {
  def apply(func: IRFunc): PContext = new PContext(func, MMap())
}
