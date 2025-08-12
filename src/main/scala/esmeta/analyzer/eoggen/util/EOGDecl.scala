package esmeta.analyzer.eoggen.util

import esmeta.analyzer.eoggen.*
import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.HtmlUtils.escapeES

trait EOGDecl { self: Self =>

  // EOG (Execution Order Graph) representation
  case class EOG(
    nodes: Seq[ControlPoint],
    edges: Seq[(ControlPoint, ControlPoint)],
  ):
    lazy val dot: DotFile[ControlPoint] = DotFile[ControlPoint](nodes, edges)

    lazy val simplified: EOG = this
  end EOG

  lazy val eog: EOG = {
    self.analyze
    val nodes = {
      (npMap.keySet ++ rpMap.keySet)
    }.toSeq

    val edges: Seq[(ControlPoint, ControlPoint)] = {
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
    }.toSeq

    EOG(nodes, edges)
  }

  /** type class for Dottable */
  given dottable_eog: Dottable[ControlPoint] = new Dottable[ControlPoint] {

    extension (x: ControlPoint)
      def id: String = x match
        case NodePoint(func, node, view) =>
          s"\"${func.id}:${node.id}:${view.toString()}\""
        case ReturnPoint(func, view) => s"\"${func.id}:return\""

      def label: String = x match
        case NodePoint(func, node, view) =>
          s"${func.id}:${node.id}:${view.toString()} - ${node.toString()}".escapeES
        case ReturnPoint(func, view) => s"${func.id}:return".escapeES

      def color: DotFile.Color = DotFile.Color.Black
      def bgColor: DotFile.Color =
        if x == initialNp then DotFile.Color.Cyan
        else {
          x match
            case NodePoint(_, _: Branch, _) => DotFile.Color.Yellow
            case NodePoint(_, _: Call, _)   => DotFile.Color.Green
            case ReturnPoint(_, _)          => DotFile.Color.Red
            case _                          => DotFile.Color.White
        }

      override def subgraph: Option[String] = Some(
        s"${x.func.id.toString()}${x.view.toString()}",
      )

      override def shape: DotFile.Shape = x match
        case NodePoint(_, node, _) =>
          node match
            case _: Block  => DotFile.Shape.Box
            case _: Call   => DotFile.Shape.Ellipse
            case _: Branch => DotFile.Shape.Diamond
        case _: ReturnPoint => DotFile.Shape.Ellipse

      override def edgeLabel(y: ControlPoint): String = (x, y) match
        case ((_: ReturnPoint), (_: NodePoint[?])) => "return"
        case _                                     => ""

    end extension
  }

}
