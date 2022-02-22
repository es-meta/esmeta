package esmeta.cfg.util

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc}
import esmeta.ir.util.*
import esmeta.spec.{Spec, TypeModel, Grammar}
import scala.collection.mutable.Queue

/** extensions for control-flow graphs (CFGs) */
extension (cfg: CFG) {

  /** get a type model */
  def typeModel: TypeModel = spec.typeModel

  /** get the corresponding specification */
  def spec: Spec = cfg.program.spec

  /** get the corresponding grammar */
  def grammar: Grammar = spec.grammar
}

/** extensions for functions */
extension (func: Func) {

  /** check whether it is builtin */
  def isBuiltin: Boolean = func.ir.kind == IRFunc.Kind.Builtin
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
