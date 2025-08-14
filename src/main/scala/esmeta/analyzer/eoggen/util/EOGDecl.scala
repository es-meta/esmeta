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

trait EOGDecl { self: Self =>

  // EOG (Execution Order Graph) representation
  case class EOG(
    nodes: Set[ControlPoint],
    edges: Set[(ControlPoint, ControlPoint)],
  ) {
    lazy val dot: DotFile[ControlPoint] =
      DotFile[ControlPoint](nodes.toSeq, edges.toSeq)

    lazy val simplified: EOG = Reducers(this)
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

  object Reducers {
    def apply(eog: EOG): EOG = fix(reduce)(eog)
    private def reduce = reduceNode >>> reduceBranch

    /* reduce p => node -> c as p => c, when node is not marked */
    private def reduceNode(eog: EOG): EOG = {
      val target = (for {
        np <- eog.nodes
        cSet = eog.edges.collect { case `np` -> child => child }
        if (cSet.size == 1) // only one child
        child <- cSet
        if !np.marked
      } yield np -> child).headOption

      for { (np -> child) <- target } yield eog.copy(
        nodes = eog.nodes - np,
        edges = eog.edges.flatMap {
          case `np` -> `child` => None
          case parent -> `np`  => Some(parent -> child)
          case fallback        => Some(fallback)
        },
      )
    }.getOrElse(eog)

    /* reduce 'foldable' branches
     *
     * TODO handle 'marked' branches correctly
     */
    private def reduceBranch(eog: EOG): EOG = {
      val target = (for {
        np <- eog.nodes
        cSet = eog.edges.collect { case `np` -> child => child }
        if cSet.size == 2 // only two children
        arbitrary <- cSet // iterate all cases
        // assert : np -> arbitrary
        ccSet = eog.edges.collect { case `arbitrary` -> child => child }
        if ccSet.size == 2 // two grand-children (branch)
        if (eog.nodes.exists(cp =>
          eog.edges.contains(np -> cp) &&
          eog.edges.contains(arbitrary -> cp),
        ))
        // assert : np ------------> cp
        //           \-> arbitrary /^
      } yield np -> arbitrary).headOption

      for { (np -> arbitrary) <- target } yield
        val edges = eog.edges.flatMap {
          case `np` -> `arbitrary` => None
          case parent -> `np`      => Some(parent -> arbitrary)
          case fallback            => Some(fallback)
        }
        eog.copy(
          nodes = edges.flatMap { case p -> c => List(p, c) },
          edges = edges,
        )
    }.getOrElse(eog)
  }

  extension (cp: ControlPoint)
    def marked = cp match
      case NodePoint(_, node: Call, view) =>
        node.callInst match {
          case sdo: ISdoCall
              if sdo.op == "Evaluation" &&
              npMap.keySet.forall { _.view != view + node } =>
            true
          case _ => false
        }
      case _ => false
  end extension

  /** type class for Dottable */
  given dottable_eog: Dottable[ControlPoint] = new Dottable[ControlPoint] {

    extension (x: ControlPoint)
      def id: String = x match
        case NodePoint(func, node, view) =>
          s"\"${func.id}:${node.id}:${view.toString()}\""
        case ReturnPoint(func, view) => s"\"${func.id}:return\""

      def label: String = x match
        case NodePoint(func, node, view) =>
          s"${view.toString()}:${func.name}:${node.toString()}".escapeES
        case ReturnPoint(func, view) => s"${func.name}:return".escapeES

      def color: DotFile.Color = DotFile.Color.Black
      def bgColor: DotFile.Color =
        // if x == initialNp then DotFile.Color.Cyan
        // else
        if x.marked then DotFile.Color.Green
        else DotFile.Color.White
      // else {
      //   x match
      //     case NodePoint(_, _: Branch, _) => DotFile.Color.Yellow
      //     case NodePoint(_, _: Call, _)   => DotFile.Color.Green
      //     case ReturnPoint(_, _)          => DotFile.Color.Red
      //     case _                          => DotFile.Color.White
      // }

      override def subgraph: Option[String] =
        None
      // s"${x.func.id.toString()}${x.view.toString()}".some

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

  // auxiliaries

  extension [A](a: A) {
    inline def some: Some[A] = Some(a)
  }

  extension [A, B](f: A => B) {
    inline def >>>[C](g: B => C): A => C = (x: A) => g(f(x))
  }

  @targetName("fixF")
  final def fix[T](f: T => T)(x: T): T = fix(f, x)

  @tailrec
  final def fix[T](f: T => T, x: T): T = {
    val y = f(x)
    if (y == x) y else fix(f, y)
  }

}
