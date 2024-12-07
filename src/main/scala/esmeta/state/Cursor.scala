package esmeta.state

import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** IR cursors */
sealed trait Cursor extends StateElem {

  /** CFG function */
  def func: Func

  /** get source locations */
  def loc: Option[Loc] = this match
    case NodeCursor(func, node, idx) =>
      node match
        case Block(_, insts, _) => optional(insts(idx)).flatMap(_.loc)
        case _                  => node.loc
    case _ => None
}
case class NodeCursor(
  func: Func,
  node: Node,
  var idx: Int = 0, // idx for block node (used in debugger)
) extends Cursor
case class ExitCursor(func: Func) extends Cursor
object Cursor {
  def apply(nodeOpt: Option[Node], f: Func): Cursor =
    nodeOpt.fold(ExitCursor(f))(NodeCursor(f, _))
}
