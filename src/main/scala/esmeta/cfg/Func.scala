package esmeta.cfg

import esmeta.ir.{Type, Name, Func => IRFunc}
import esmeta.cfg.util.*
import esmeta.util.UId
import esmeta.util.Appender

/** CFG functions */
case class Func(
  id: Int,
  ir: IRFunc,
  entry: Option[Node],
) extends CFGElem
  with UId { func =>

  /** nodes */
  lazy val nodes: Set[Node] = entry.fold(Set())(_.reachable)

  /** a mapping from nid to nodes */
  lazy val nodeMap: Map[Int, Node] =
    (for (node <- nodes) yield node.id -> node).toMap

  /** check whether it is builtin */
  lazy val isBuiltin: Boolean = func.ir.kind == IRFunc.Kind.Builtin

  /** conversion to a DOT format */
  def toDot: String = new DotPrinter {
    def getId(func: Func): String = s"cluster${func.id}"
    def getId(node: Node): String = s"node${node.id}"
    def getName(func: Func): String = func.ir.name
    def getColor(node: Node): String = REACH
    def getColor(from: Node, to: Node): String = REACH
    def getBgColor(node: Node): String = NORMAL
    def apply(app: Appender): Unit = addFunc(func, app)
  }.toString
}
