package esmeta.cfg.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.ir.util.{UnitWalker => IRUnitWalker}
import esmeta.ir.{Func => _, *}
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.{QueueWorklist, Appender}

/** dependency graph */
class DepGraph(cfg: CFG) {

  /** control dependencies */
  val ctrlDeps = cached[Func, ControlDep](ControlDep.apply)

  /** data dependencies */
  val dataDeps = cached[Func, DataDep](DataDep.apply)

  /** direct control/data dependencies */
  val directDeps = cached[Func, Map[Node, Set[Node]]] { func =>
    val ctrl = ctrlDeps(func)
    val data = dataDeps(func)
    val deps = for {
      node <- func.nodes.toList
    } yield node -> (ctrl.deps(node) ++ data.deps(node))
    deps.toMap
  }

  /** reverse control/data dependencies */
  val revDeps = cached[Func, Map[Node, Set[Node]]] { func =>
    val direct = directDeps(func)
    var rev = Map[Node, Set[Node]]().withDefaultValue(Set())
    for {
      (from, set) <- direct
      to <- set
    } rev += to -> (rev(to) + from)
    rev
  }

  /** transitive control/data dependencies */
  val deps = cached[Func, Map[Node, Set[Node]]] { func =>
    val direct = directDeps(func)
    val rev = revDeps(func)
    var map = directDeps(func)
    val worklist = QueueWorklist(map.keys)
    while (
      worklist.next.fold(false) { node =>
        val cur = map(node)
        for (node <- rev(node)) {
          val set = map(node)
          val newSet = set ++ cur
          if (set != newSet) {
            map += node -> newSet
            worklist += node
          }
        }
        true
      }
    ) {}
    map
  }

  def getStringForm(map: Map[Node, Set[Node]]): String = {
    val app = new Appender
    given Rule[Node] = (app, node) => app >> node.id
    given Rule[Iterable[Node]] = iterableRule("[", ", ", "]")
    for { (node, set) <- map.toList.sortBy(_._1.id) } {
      app >> node >> " -> " >> set >> LINE_SEP
    }
    app.toString
  }
}

// TODO: optimize
/** control dependencies */
case class ControlDep(func: Func) {

  /** post-dominators */
  val pdom = PostDominator(func)

  /** reverse control dependencies */
  val rev: Map[Node, Set[Node]] = (for {
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
  val deps: Map[Node, Set[Node]] = {
    var map = func.nodes.toList.map(_ -> Set[Node]()).toMap
    for {
      (branch, set) <- rev
      node <- set
    } map += node -> (map(node) + branch)
    map
  }
}

// TODO: optimize
/** data dependencies */
case class DataDep(func: Func) {
  val (
    /** definitions */
    defs: Map[Node, Set[Local]],
    /** uses */
    uses: Map[Node, Set[Local]],
  ) = {
    val map = (for (node <- func.nodes) yield {
      val collector = new DefUseCollector
      node match
        case block: Block   => block.insts.map(collector.walk)
        case call: Call     => collector.walk(call.callInst)
        case branch: Branch => collector.walk(branch.cond)
      node -> collector
    }).toMap
    val defs = map.map { _ -> _.defs }.withDefaultValue(Set())
    val uses = map.map { _ -> _.uses }.withDefaultValue(Set())
    (defs, uses)
  }
  private class DefUseCollector extends IRUnitWalker {
    var defs = Set[Local]()
    var uses = Set[Local]()
    override def walk(inst: Inst): Unit = inst match
      case ILet(x: Local, expr)    => defs += x; walk(expr)
      case IAssign(x: Local, expr) => defs += x; walk(expr)
      case ICall(x: Local, f, as)  => defs += x; walk(f); walkList(as, walk)
      case ISdoCall(x: Local, b, n, as) =>
        defs += x; walk(b); walk(n); walkList(as, walk)
      case _ => super.walk(inst)
    override def walk(x: Var): Unit = x match
      case x: Local => uses += x
      case _        =>
  }

  /** use to definitions for each node */
  val useToDefs: Map[Node, Map[Local, Set[Node]]] = {
    val succs = func.succs
    var reach = Map[Node, Map[Local, Set[Node]]]().withDefaultValue(Map())
    val worklist = QueueWorklist(func.nodes)
    while (
      worklist.next.fold(false) { node =>
        val gen = defs(node).toList.map(_ -> Set(node)).toMap
        val newMap = reach(node) ++ gen
        for (succ <- func.succs(node)) {
          val oldMap = reach(succ).withDefaultValue(Set())
          if (newMap.exists { case (x, set) => !(set subsetOf oldMap(x)) }) {
            val mergedMap = newMap.foldLeft(oldMap) {
              case (map, (x, ns)) => map + (x -> (map(x) ++ ns))
            }
            reach += succ -> mergedMap
            worklist += succ
          }
        }
        true
      }
    ) {}
    (for (node <- func.nodes.toList) yield {
      val used = uses(node)
      node -> reach(node).filter { case (x, _) => used(x) }
    }).toMap
  }

  /** direct data dependencies */
  val deps: Map[Node, Set[Node]] = useToDefs
    .map { case (node, map) => node -> map.values.flatten.toSet }
}

// TODO: optimize
/** post-dominators */
case class PostDominator(func: Func) {
  def apply(node: Node): Set[Node] = map.getOrElse(node, Set())
  val map: Map[Node, Set[Node]] = {
    val exits = func.exits
    val succs = func.succs
    val nonExits = func.nodes -- exits
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
