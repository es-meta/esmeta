package esmeta.cfg.util

import esmeta.cfg.*
import esmeta.util.QueueWorklist
import esmeta.util.BaseUtils.*

/** dependency graph */
class DepGraph(cfg: CFG) {

  /** control dependencies */
  val controlDep = cached[Func, ControlDep](ControlDep.apply)
}

// TODO: optimize
/** control dependencies */
case class ControlDep(func: Func) {

  /** post-dominators */
  val pdom = PostDominator(func)

  /** reverse control dependencies */
  val rev: Map[Branch, Set[Node]] = (for {
    node <- func.nodes
    branch <- node match
      case branch: Branch => Some(branch)
      case _              => None
    set = branch.thenNode.fold(Set())(pdom.apply)
      ++ branch.elseNode.fold(Set())(pdom.apply)
      -- pdom(branch)
      - branch
  } yield branch -> set).toMap

  /** direct control dependencies */
  val direct: Map[Node, Set[Branch]] = {
    var map = Map[Node, Set[Branch]]().withDefaultValue(Set())
    for {
      (branch, set) <- rev
      node <- set
    } map += node -> (map(node) + branch)
    map
  }

  /** indirect control dependencies */
  val indirect: Map[Node, Set[Branch]] = {
    var map = direct
    val worklist = QueueWorklist(direct.keys.collect { case b: Branch => b })
    while (
      worklist.next.fold(false) { branch =>
        val cur = map(branch)
        for (node <- rev(branch)) {
          val set = map(node)
          val newSet = set ++ cur
          if (set != newSet) {
            map += node -> newSet
            node match {
              case branch: Branch => worklist += branch
              case _              =>
            }
          }
        }
        true
      }
    ) {}
    map
  }
}

// TODO: optimize
/** post-dominators */
case class PostDominator(func: Func) {
  def apply(node: Node): Set[Node] = map.getOrElse(node, Set())
  val map: Map[Node, Set[Node]] = {
    val entry = func.entry
    val nodes = func.nodes
    val exits = func.exits
    val preds = func.preds
    val succs = func.succs
    val nonExits = nodes -- exits
    var pdom = exits.map(x => x -> Set(x)).toMap.withDefaultValue(Set())
    var changed = true
    while (changed) {
      changed = false
      for (node <- nonExits) {
        val newSet = succs(node).map(pdom).reduce(_ & _) + node
        if (pdom(node) != newSet) {
          pdom += node -> newSet
          changed = true
        }
      }
    }
    pdom
  }
}
