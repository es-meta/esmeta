package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.util.Appender

class Graph(
  sem: AbsSemantics,
  cur: Option[ControlPoint],
  depthOpt: Option[Int],
  pathOpt: Option[Path],
) {

  /** conversion to a DOT format */
  lazy val toDot: String =
    given app: Appender = new Appender
    (app >> "digraph").wrap {
      app :> """graph [fontname = "Consolas"]"""
      app :> """node [fontname = "Consolas"]"""
      app :> """edge [fontname = "Consolas"]"""
      (cur, depthOpt, pathOpt) match
        case (Some(cp), _, Some(path)) =>
          var calleePrinter = DotPrinter(cp, cur)
          calleePrinter.addTo(app)
          for (callerNp <- path)
            val callerPrinter = DotPrinter(callerNp)
            callerPrinter.addTo(app)
            drawCall(callerPrinter, callerNp.node, calleePrinter)
            calleePrinter = callerPrinter
        case (Some(cp), Some(depth), _) =>
          var printer = DotPrinter(cp, cur)
          printer.addTo(app)
          showPrev(printer, depth)
        case _ =>
          var visited = Set[ReturnPoint]()
          for {
            (calleeRp, callerNps) <- sem.retEdges
            callerNp <- callerNps
            calleePrinter = DotPrinter(calleeRp, cur)
            callerPrinter = DotPrinter(callerNp, cur)
            callerRp = callerPrinter.rp
          } {
            if (!visited.contains(calleeRp))
              visited += calleeRp
              calleePrinter.addTo(app)
            if (!visited.contains(callerRp))
              visited += callerRp
              callerPrinter.addTo(app)
            drawCall(callerPrinter, callerNp.node, calleePrinter)
          }
    }
    app.toString

  // show previous traces with depth
  def showPrev(
    printer: DotPrinter,
    depth: Int,
  )(using app: Appender): Unit =
    var visited = Set[ReturnPoint](printer.rp)
    def aux(calleePrinter: DotPrinter, depth: Int): Unit = if (depth > 0) {
      val calleeRp = calleePrinter.rp
      for (callerNp @ NodePoint(_, call, callView) <- sem.getRetEdges(calleeRp))
        val callerPrinter = DotPrinter(callerNp)
        val callerRp = callerPrinter.rp
        if (!visited.contains(callerRp))
          visited += callerRp
          callerPrinter.addTo(app)
          aux(callerPrinter, depth - 1)
        drawCall(callerPrinter, callerNp.node, calleePrinter)
    }
    aux(printer, depth)

  // draw call edges
  def drawCall(
    callerPrinter: DotPrinter,
    call: Call,
    calleePrinter: DotPrinter,
  )(using app: Appender): Unit =
    val cid = callerPrinter.getId(call)
    val eid = calleePrinter.entryId
    app :> s"$cid -> $eid [label=call]"
}

// path type
type Path = List[NodePoint[Call]]
