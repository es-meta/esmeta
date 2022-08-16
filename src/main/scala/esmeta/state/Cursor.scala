package esmeta.state

import esmeta.cfg.*

/** IR cursors */
sealed trait Cursor extends StateElem {
  var idx: Int = 0 // idx for block node (used in debugger)
}
case class NodeCursor(node: Node) extends Cursor
case class ExitCursor(func: Func) extends Cursor
object Cursor {
  def apply(nodeOpt: Option[Node], f: Func): Cursor =
    nodeOpt.fold(ExitCursor(f))(NodeCursor(_))
}
