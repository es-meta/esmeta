package esmeta.peval.pstate

import esmeta.ir.{Local, Expr, Func => IRFunc}
import esmeta.peval.domain.*
import esmeta.state.{StateElem, Value}
import scala.collection.mutable.{Map => MMap}

import esmeta.cfg.{Func, Block, Call}
import esmeta.ir.{Func => IRFunc, *}
import esmeta.es.Ast
import esmeta.util.BaseUtils.error

import esmeta.state.*
import esmeta.peval.*

/** IR contexts */
case class PContext(
  val func: Func,
  val locals: MMap[Local, Value] = MMap(),
) extends StateElem {

  /** current cursor in this context */
  var cursor: Cursor = NodeCursor(func.entry)

  /** move cursor to next */
  def moveNext: Unit = cursor match
    case NodeCursor(block: Block) => cursor = Cursor(block.next, func)
    case NodeCursor(call: Call)   => cursor = Cursor(call.next, func)
    case _                        => cursor = ExitCursor(func)

  /** return variable */
  var retVal: Option[(Return, Value)] = None

  /** copy contexts */
  def copied: PContext = {
    val newContext = copy(locals = MMap.from(locals))
    newContext.cursor = cursor
    newContext
  }

  /** name */
  def name: String = func.irFunc.name

  /** ast of current context */
  def astOpt: Option[Ast] =
    if (func.isSDO) Some(locals(NAME_THIS).asAst)
    else None
}

object PContext {
  def fromContext(ctxt: Context): PContext =
    PContext(
      func = ctxt.func,
      locals = ctxt.locals.clone(),
    )
}
