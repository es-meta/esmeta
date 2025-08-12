package esmeta.analyzer.eoggen.util

import esmeta.analyzer.eoggen.*
import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.HtmlUtils.escapeES

trait EOGDecl { self: Self =>

  enum EOGNode:
    case CP(cp: ControlPoint)
    case AfterCall(np: NodePoint[Call])

  // EOG (Execution Order Graph) representation
  case class EOG(
    nodes: Seq[EOGNode],
    edges: Seq[(EOGNode, EOGNode)],
  ):
    lazy val dot: DotFile[EOGNode] = DotFile[EOGNode](nodes, edges)

    lazy val simplified: EOG = ???
  end EOG

  given dottable_eog: Dottable[EOGNode] = new Dottable[EOGNode] {
    import EOGNode.*

    extension (x: EOGNode)
      def id: String = x match
        case CP(NodePoint(func, node, view)) =>
          s"\"${func.id}:${node.id}:${view.toString()}\""
        case CP(ReturnPoint(func, view)) => s"\"${func.id}:return\""
        case AfterCall(NodePoint(func, node, view)) =>
          s"\"${func.id}:${node.id}:${view.toString()}-aftercall\""

      def label: String = x match
        case CP(NodePoint(func, node, view)) =>
          s"${func.id}:${node.id}:${view.toString()} - ${node.toString()}".escapeES
        case CP(ReturnPoint(func, view)) => s"${func.id}:return"
        case AfterCall(NodePoint(func, node, view)) =>
          s"${func.id}:${node.id}:${view.toString()} - ${node.toString()} (after call)".escapeES

      def color: DotFile.Color = DotFile.Color.Black
      def bgColor: DotFile.Color =
        if x == CP(initialNp) then DotFile.Color.Cyan
        else {
          x match
            case CP(NodePoint(_, _: Branch, _)) => DotFile.Color.Yellow
            case CP(NodePoint(_, _: Call, _))   => DotFile.Color.Green
            case CP(ReturnPoint(_, _))          => DotFile.Color.Red
            case _                              => DotFile.Color.White
        }

      override def subgraph: Option[String] = x match
        case CP(cp) =>
          Some(
            s"${cp.func.id.toString()}${cp.view.toString()}",
          )
        case AfterCall(np) =>
          Some(
            s"${np.func.id.toString()}${np.view.toString()}",
          )

      override def shape: DotFile.Shape = x match
        case CP(NodePoint(_, node, _)) =>
          node match
            case _: Block  => DotFile.Shape.Box
            case _: Call   => DotFile.Shape.Ellipse
            case _: Branch => DotFile.Shape.Diamond
        case CP(ReturnPoint(_, _)) => DotFile.Shape.Ellipse
        case AfterCall(np)         => DotFile.Shape.Ellipse

      override def edgeLabel(y: EOGNode): String = (x, y) match
        case (CP(_: ReturnPoint), CP(_: NodePoint[?])) => "return"
        case _                                         => ""

    end extension
  }

  lazy val eog: EOG = {
    import EOGNode.*

    self.analyze
    val nodes = {
      (npMap.keySet ++ rpMap.keySet).map(EOGNode.CP.apply)
      ++
      (npMap.keySet
        .filter(_.node.isInstanceOf[Call])
        .map(np => AfterCall(np.asInstanceOf[NodePoint[Call]])))
    }.toSeq

    val edges: Seq[(EOGNode, EOGNode)] = {
      // next nodes, except call nodes
      (for {
        np <- npMap.keys
        NodePoint(func, node, view) = np
        if (!node.isInstanceOf[Call])
        succ <- node.succs
        nextNp = transfer.getNextNp(np, succ)
        if reachable(nextNp)
      } yield List(CP(np) -> CP(nextNp)))
      ++
      // call to aftercall, only if no-inline
      (for {
        np <- npMap.keys
        if (np.node.isInstanceOf[Call])
        NodePoint(func, node, view) = np
        nextView = view + node.asInstanceOf[Call]
        if (npMap.keySet.forall { _.view != nextView }) // no-inline
      } yield List(CP(np) -> AfterCall(np.asInstanceOf[NodePoint[Call]])))
      ++
      (for {
        np <- npMap.keys
        NodePoint(func, node, view) = np
        if (node.isInstanceOf[Call])
        // call to aftercall
        succ <- node.succs
        nextNp = transfer.getNextNp(np, succ)
      } yield List(
        AfterCall(np.asInstanceOf[NodePoint[Call]]) -> CP(nextNp),
      ))
      ++
      // return point to call node
      (for {
        (rp, cps) <- retEdges
        cp <- cps
        ReturnPoint(func, view) = rp
        if reachable(cp)
      } yield List(CP(rp) -> AfterCall(cp))) ++
      // call edges
      (for {
        (cp, _) <- callInfo
        (np, _) <- npMap
        NodePoint(caller, callNode, callerView) = cp
        NodePoint(callee, node, calleeView) = np
        if (node == callee.entry)
        if (calleeView == callerView + callNode)
      } yield List(CP(cp) -> CP(np)))
      ++
      // return points
      (for {
        np <- npMap.keys
        NodePoint(func, node, view) = np
        shouldExit = node match
          case Block(_, _, next) => next.isEmpty
          case Call(_, _, next)  => next.isEmpty
          case _: Branch         => false
        if shouldExit
        rp = ReturnPoint(func, view)
      } yield List(CP(np) -> CP(rp)))
    }.flatten.toSeq

    EOG(nodes, edges)
  }

}
