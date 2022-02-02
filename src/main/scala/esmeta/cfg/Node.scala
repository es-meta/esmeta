package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.util.{UId, Locational}
import scala.collection.mutable.ListBuffer

// CFG nodes
sealed trait Node extends CFGElem with UId with Locational
object Node extends Parser.From[Node]

/** block nodes */
case class Block(
  id: Int,
  var insts: ListBuffer[Inst] = ListBuffer(),
  var next: Option[Node] = None,
) extends Node

/** call nodes */
case class Call(
  id: Int,
  lhs: Id,
  fexpr: Expr,
  args: List[Expr],
  var next: Option[Node] = None,
) extends Node

/** branch nodes */
case class Branch(
  id: Int,
  kind: Branch.Kind,
  cond: Expr,
  var thenNode: Option[Node] = None,
  var elseNode: Option[Node] = None,
) extends Node
object Branch:
  enum Kind extends CFGElem:
    case If
    case Loop(str: String)
  object Kind extends Parser.From[Kind]
