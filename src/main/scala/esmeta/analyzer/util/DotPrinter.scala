package esmeta.analyzer.util

import esmeta.ANALYZE_LOG_DIR
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.cfg.util.{DotPrinter => CFGDotPrinter}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.util.Appender
import scala.Console.*

trait DotPrinterDecl { self: Self =>

  class DotPrinter(
    cp: ControlPoint,
    cur: Option[ControlPoint] = None,
  ) extends CFGDotPrinter(cp.func) {
    val WORKLIST = """"gray""""

    // helpers for views
    lazy val view = getEntryView(cp.view)
    lazy val viewStr: String = norm(view)
    lazy val rp: ReturnPoint = ReturnPoint(func, view)
    def getNp[T <: Node](node: T): NodePoint[T] =
      NodePoint(cfg.funcOf(node), node, view)

    // helpers for functions
    lazy val func = cp.func
    override lazy val funcId: String = s"cluster${func.id}_$viewStr"
    override lazy val funcName: String =
      norm(func.headString) + (if (view.isEmpty) "" else s" [VIEW: $viewStr]")

    // helpers for function exit
    override lazy val exitColor: String =
      if (reachable(rp)) REACH else NON_REACH
    override lazy val exitBgColor: String =
      if (cur == Some(rp)) CURRENT
      else if (worklist has rp) WORKLIST
      else NORMAL
    override def exitEdgeColor(from: Node): String =
      if (reachable(getNp(from)) && reachable(rp)) REACH
      else NON_REACH

    // helpers for nodes
    override def getId(node: Node): String = s"node${node.id}_$viewStr"
    override def getColor(node: Node): String =
      if (reachable(getNp(node))) REACH else NON_REACH
    override def getBgColor(node: Node): String =
      val np = getNp(node)
      if (cur == Some(np)) CURRENT
      else if (worklist has np) WORKLIST
      else NORMAL
    override def getEdgeColor(from: Node, to: Node): String =
      if (reachable(getNp(from)) && reachable(getNp(to))) REACH
      else NON_REACH

    // normalize strings for view
    private val normPattern = """[-:\[\](),\s~?"]""".r
    protected def norm(view: View): String =
      normPattern.replaceAllIn(view.toString, "_")
  }
  object DotPrinter {
    // path for CFG
    val CFG_PATH = s"$ANALYZE_LOG_DIR/cfg"

    // dump CFG in DOT/PDF format
    def dumpCFG(
      cp: Option[ControlPoint] = None,
      pdf: Boolean = true,
      depth: Option[Int] = None,
      path: Option[Path] = None,
    ): Unit = try {
      dumpDot(Graph(cp, depth, path).toDot, pdf)
    } catch {
      case _: Throwable => printlnColor(RED)(s"Cannot dump CFG")
    }

    // dump CFG function in a DOT/PDF format
    def dumpFunc(
      func: Func,
      pdf: Boolean = true,
    ): Unit = try {
      dumpDot(func.toDot(), pdf)
    } catch {
      case _: Throwable => printlnColor(RED)(s"Cannot dump CFG function")
    }

    // dump DOT
    def dumpDot(dot: String, pdf: Boolean): Unit =
      dumpFile(dot, s"$CFG_PATH.dot")
      if (pdf) {
        executeCmd(
          s"""unflatten -l 10 -o ${CFG_PATH}_trans.dot $CFG_PATH.dot""",
        )
        executeCmd(s"""dot -Tpdf "${CFG_PATH}_trans.dot" -o "$CFG_PATH.pdf"""")
        println(s"Dumped CFG to $CFG_PATH.pdf")
      } else println(s"Dumped CFG to $CFG_PATH.dot")
  }
}
