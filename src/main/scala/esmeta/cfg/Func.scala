package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.util.UId
import esmeta.util.Appender

// CFG functions
case class Func(
  id: Int,
  main: Boolean,
  kind: Func.Kind,
  name: String,
  params: List[Func.Param],
  entry: Option[Node],
) extends CFGElem
  with UId { func =>
  lazy val nodes: Set[Node] = entry.fold(Set())(reachable)
  lazy val nodeMap: Map[Int, Node] =
    (for (node <- nodes) yield node.id -> node).toMap
  lazy val toDot: String = (new DotPrinter {
    def getId(func: Func): String = s"cluster${func.id}"
    def getId(node: Node): String = s"node${node.id}"
    def getName(func: Func): String = func.name
    def getColor(node: Node): String = REACH
    def getColor(from: Node, to: Node): String = REACH
    def getBgColor(node: Node): String = NORMAL
    def apply(app: Appender): Unit = addFunc(func, app)
  }).toString
}
object Func extends Parser.From[Func] {
  enum Kind extends CFGElem:
    case AbsOp, NumMeth, SynDirOp, ConcMeth, InternalMeth, Builtin, Clo, Cont
  object Kind extends Parser.From[Kind]

  /** function parameters */
  case class Param(
    lhs: Name,
    optional: Boolean = false,
    ty: Type = Type("Any"),
  ) extends CFGElem
  object Param extends Parser.From[Param]
}
