package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.util.Appender

case class Graph(
  sem: AbsSemantics,
  curOpt: Option[ControlPoint],
  depthOpt: Option[Int],
  pathOpt: Option[Path],
) {
  // conversion to DOT
  lazy val toDot: String =
    val app = new Appender
    (app >> "digraph").wrap {
      app :> """graph [fontname = "Consolas"]"""
      app :> """node [fontname = "Consolas"]"""
      app :> """edge [fontname = "Consolas"]"""
      (curOpt, depthOpt, pathOpt) match
        case (Some(cp), _, Some(path)) =>
          val func = cp.func
          val view = sem.getEntryView(cp.view)
          val dot = ViewDotPrinter(view)
          val entry = func.entry
          var eid = dot.getId(entry)
          dot.addFunc(func, app)
          for (np <- path)
            val func = np.func
            val view = sem.getEntryView(np.view)
            val dot = ViewDotPrinter(view)
            dot.addFunc(func, app)
            dot.drawCall(np.node, eid, app)
            val entry = func.entry
            eid = dot.getId(entry)
        case (Some(cp), Some(depth), _) =>
          val func = cp.func
          val view = sem.getEntryView(cp.view)
          val isExit =
            (sem.worklist has ReturnPoint(func, view)) || (cp.equals(
              ReturnPoint(func, view),
            ))
          val dot = ViewDotPrinter(
            view,
            isExit,
          )
          val rp = ReturnPoint(func, view)
          dot.addFunc(func, app)
          showPrev(rp, dot, depth, app)
        case _ =>
          val funcs: Set[(Func, View)] =
            sem.npMap.keySet.map(np => (np.func, sem.getEntryView(np.view)))
          for ((func, view) <- funcs)
            val isExit =
              (sem.worklist has ReturnPoint(func, view)) || (sem.curCp match {
                case Some(x) => x.equals(ReturnPoint(func, view))
                case None    => false
              })
            val dot = ViewDotPrinter(view, isExit)
            dot.addFunc(func, app)
          // print call edges
          for ((ReturnPoint(func, returnView), calls) <- sem.retEdges)
            val entry = func.entry
            val eid = ViewDotPrinter(returnView).getId(entry)
            for (callNp @ NodePoint(_, call, callView) <- calls)
              ViewDotPrinter(sem.getEntryView(callView))
                .drawCall(call, eid, app),
    }
    app.toString

  // show previous traces with depth
  def showPrev(
    rp: ReturnPoint,
    dot: DotPrinter,
    depth: Int,
    app: Appender,
  ): Unit =
    var visited = Set[ReturnPoint]()
    def aux(
      rp: ReturnPoint,
      dot: DotPrinter,
      depth: Int,
    ): Unit = if (depth > 0) {
      val entry = rp.func.entry
      val entryNp = NodePoint(_, entry, rp.view)
      val eid = dot.getId(entry)
      for (callNp @ NodePoint(_, call, callView) <- sem.getRetEdges(rp))
        val func = callNp.func
        val entryView = sem.getEntryView(callView)
        val callRp = ReturnPoint(func, entryView)
        val callDot = ViewDotPrinter(entryView)
        if (!(visited contains callRp))
          visited += callRp
          callDot.addFunc(func, app)
          aux(callRp, callDot, depth - 1)
        callDot.drawCall(call, eid, app)
    }
    aux(rp, dot, depth)

  private case class ViewDotPrinter(
    view: View,
    isExit: Boolean = false,
  ) extends DotPrinter {
    def getId(func: Func): String = s"cluster${func.id}_${norm(view)}"
    def getId(node: Node): String = s"node${node.id}_${norm(view)}"
    def getName(func: Func): String =
      val funcName = func.headString.replaceAll("\"", "\\\\\"")
      val viewName =
        if (view.isEmpty) ""
        else " [VIEW: " + view.toString.replaceAll("\"", "\\\\\"") + "]"
      s"$funcName$viewName"
    def getColor(node: Node): String =
      val np = NodePoint(cfg.funcOf(node), node, view)
      if (sem.reachable(np)) REACH
      else NON_REACH
    def getColor(from: Node, to: Node): String =
      val fromNP = NodePoint(cfg.funcOf(from), from, view)
      val toNP = NodePoint(cfg.funcOf(to), to, view)
      if (sem.reachable(fromNP) && sem.reachable(toNP)) REACH
      else NON_REACH
    def getBgColor(node: Node): String =
      val np = NodePoint(cfg.funcOf(node), node, view)
      val nps = sem.getNps(np).toSet[ControlPoint]
      if (curOpt.fold(false)(nps contains _)) CURRENT
      else if (nps.exists(sem.worklist has _)) SELECTED
      else NORMAL
    def apply(app: Appender): Unit = {}
    def drawCall(
      call: Call,
      entryId: String,
      app: Appender,
    ): Unit = drawEdge(getId(call), entryId, REACH, Some("call"), app)
  }
}

// path type
type Path = List[NodePoint[Call]]
