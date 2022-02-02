package esmeta.cfg.util

import esmeta.cfg.*
import scala.collection.mutable.Queue

/** extensions for functions */
extension (func: Func) {

  /** check whether it is builtin */
  def isBuiltin: Boolean = func.kind == Func.Kind.Builtin
}

/** get reachable nodes */
def reachable(node: Node): Set[Node] = {
  var visited = Set[Node](node)
  var queue = Queue[Node](node)
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
