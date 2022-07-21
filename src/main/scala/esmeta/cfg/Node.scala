package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.util.{UId, Locational}
import esmeta.ir.*
import scala.collection.mutable.{Queue, ListBuffer}

// CFG nodes
sealed trait Node extends CFGElem with UId {

  /** marker for loop predecessor for loop sensitivity */
  var isLoopPred: Boolean = false

  /** get reachable nodes */
  def reachable: Set[Node] =
    var visited = Set[Node](this)
    var queue = Queue[Node](this)
    def add(nodeOpt: Option[Node]): Unit = nodeOpt.map { node =>
      if (!visited.contains(node)) { queue.enqueue(node); visited += node }
    }
    while (!queue.isEmpty) {
      val cur = queue.dequeue
      cur match
        case block: Block   => add(block.next)
        case call: Call     => add(call.next)
        case branch: Branch => add(branch.thenNode); add(branch.elseNode)
    }
    visited
}

/** block nodes */
case class Block(
  id: Int,
  var insts: ListBuffer[NormalInst] = ListBuffer(),
  var next: Option[Node] = None,
) extends Node

/** nodes with backward edge to inst */
// TODO refactor
sealed trait NodeWithInst extends Node {
  var inst: Option[Inst] = None
  def setInst(i: Inst): this.type = { this.inst = Some(i); this }
}

/** call nodes */
case class Call(
  id: Int,
  callInst: CallInst,
  var next: Option[Node] = None,
) extends NodeWithInst {
  override val inst: Option[Inst] = Some(callInst)
  def lhs: Local = callInst.lhs
}

/** branch nodes */
case class Branch(
  id: Int,
  kind: Branch.Kind,
  cond: Expr,
  var thenNode: Option[Node] = None,
  var elseNode: Option[Node] = None,
) extends NodeWithInst {
  def isLoop: Boolean = kind match
    case Branch.Kind.If => false
    case _              => true
}
object Branch:
  enum Kind extends CFGElem:
    case If
    case Loop(str: String)
