package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.util.UId

// CFG functions
case class Func(
  id: Int,
  main: Boolean,
  kind: Func.Kind,
  name: String,
  params: List[Func.Param],
  entry: Option[Node],
) extends CFGElem
  with UId {
  lazy val nodes: Set[Node] = entry.fold(Set())(reachable)
  lazy val nodeMap: Map[Int, Node] =
    (for (node <- nodes) yield node.id -> node).toMap
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
