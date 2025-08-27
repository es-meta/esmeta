package esmeta.analyzer.eoggen.util

import esmeta.analyzer.eoggen.*
import esmeta.cfg.*
import esmeta.ir.{ISdoCall}
import esmeta.util.*
import esmeta.util.BaseUtils.raise
import esmeta.util.HtmlUtils.escapeES
import scala.annotation.{tailrec, targetName}
import esmeta.util.SystemUtils.dumpFile
import scala.util.chaining.*
import esmeta.util.HtmlUtils.escapeHtml

trait EOGDecl { self: Self =>

  // EOG (Execution Order Graph) representation
  case class EOG(
    nodes: Set[ControlPoint],
    edges: Set[(ControlPoint, ControlPoint)],
  ) {
    lazy val dot: DotFile[ControlPoint] =
      DotFile[ControlPoint](nodes.toSeq, edges.toSeq)

    lazy val simplified: EOG = Reducer(this)
  }

  lazy val eog: EOG = {
    self.analyze
    val nodes = (npMap.keySet ++ rpMap.keySet)

    val edges: Set[(ControlPoint, ControlPoint)] = {
      // next nodes, except call nodes
      val x1: Iterable[(ControlPoint, ControlPoint)] = (for {
        np <- npMap.keys
        NodePoint(func, node, view) = np
        if (!node.isInstanceOf[Call])
        succ <- node.succs
        nextNp = transfer.getNextNp(np, succ)
        if reachable(nextNp)
      } yield np -> nextNp)
      // ++
      // call to aftercall, only if no-inline
      val x2 = (for {
        np <- npMap.keys
        if (np.node.isInstanceOf[Call])
        NodePoint(func, node, view) = np
        nextView = view + node.asInstanceOf[Call]
        if (npMap.keySet.forall { _.view != nextView }) // no-inline
        succ <- node.succs
        nextNp = transfer.getNextNp(np, succ)
      } yield np -> nextNp)
      // ++
      // return point to succ of call node
      val x3 = (for {
        (rp, cps) <- retEdges
        cp <- cps
        ReturnPoint(func, view) = rp
        if reachable(cp)
        NodePoint(_, node, _) = cp
        succ <- node.succs
        nextNp = transfer.getNextNp(cp, succ)
      } yield rp -> nextNp)
      // ++
      // call edges
      val x4 = (for {
        (cp, _) <- callInfo
        (np, _) <- npMap
        NodePoint(caller, callNode, callerView) = cp
        NodePoint(callee, node, calleeView) = np
        if (node == callee.entry)
        if (calleeView == callerView + callNode)
      } yield cp -> np)
      // ++
      // return points
      val x5 = (for {
        np <- npMap.keys
        NodePoint(func, node, view) = np
        shouldExit = node match
          case Block(_, _, next) => next.isEmpty
          case Call(_, _, next)  => next.isEmpty
          case _: Branch         => false
        if shouldExit
        rp = ReturnPoint(func, view)
      } yield np -> rp)

      x1 ++ x2 ++ x3 ++ x4 ++ x5
    }.toSet

    EOG(nodes, edges)
  }

  // auxiliaries

  extension (cp: ControlPoint) {
    def marked = cp match
      case np @ NodePoint(_, node: Call, _) =>
        holeSdoInfo.contains(np.asInstanceOf[NodePoint[Call]])
      case _ => false
  }

  final def fix[T](f: T => T)(x: T): T =
    @tailrec def fixT[T](f: T => T, x: T): T = {
      val fx = f(x); if (fx == x) fx else fixT(f, fx)
    }
    fixT(f, x)

}
