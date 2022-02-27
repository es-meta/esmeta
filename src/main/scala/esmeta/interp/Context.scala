package esmeta.interp

import esmeta.cfg.Func
import esmeta.ir.{Func => IRFunc, *}
import scala.collection.mutable.{Map => MMap}

/** IR contexts */
case class Context(
  val func: Func,
  val locals: MMap[Local, Value] = MMap(),
) extends InterpElem {

  /** current cursor in this context */
  var cursor: Cursor = func.entry.fold(ExitCursor(func))(NodeCursor(_))

  /** return variable */
  var retVal: Option[Value] = None

  /** copy contexts */
  def copied: Context = {
    val newContext = copy(locals = MMap.from(locals))
    newContext.cursor = cursor
    newContext
  }

  /** name */
  def name: String = func.irFunc.name
}
