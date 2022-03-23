package esmeta.interp

import esmeta.cfg.*

/** IR cursors */
sealed trait Cursor extends InterpElem
case class NodeCursor(node: Node) extends Cursor
case class ExitCursor(func: Func) extends Cursor
object Cursor {
  def apply(nodeOpt: Option[Node], f: Func): Cursor =
    nodeOpt.fold(ExitCursor(f))(NodeCursor(_))
}
