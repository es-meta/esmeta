package esmeta.analyzer.util

import esmeta.*
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.cfg.util.{DotPrinter => CFGDotPrinter}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.util.Appender
import scala.Console.*

trait DotPrinter extends CFGDotPrinter {
  val SELECTED = """"gray""""

  // normalize strings for view
  private val normPattern = """[-:\[\](),\s~?"]""".r
  protected def norm(view: View): String = {
    normPattern.replaceAllIn(view.toString, "_")
  }
}
object DotPrinter {

  val CFG_PATH = s"$ANALYZE_LOG_DIR/cfg"

  // dump CFG in DOT/PDF format
  def dumpCFG(
    sem: AbsSemantics,
    cp: Option[ControlPoint] = None,
    pdf: Boolean = true,
    depth: Option[Int] = None,
    path: Option[Path] = None,
  ): Unit = try {
    dumpDot(Graph(sem, cp, depth, path).toDot, pdf)
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
  def dumpDot(dot: String, pdf: Boolean): Unit = {
    dumpFile(dot, s"$CFG_PATH.dot")
    if (pdf) {
      executeCmd(s"""unflatten -l 10 -o ${CFG_PATH}_trans.dot $CFG_PATH.dot""")
      executeCmd(s"""dot -Tpdf "${CFG_PATH}_trans.dot" -o "$CFG_PATH.pdf"""")
      println(s"Dumped CFG to $CFG_PATH.pdf")
    } else println(s"Dumped CFG to $CFG_PATH.dot")
  }
}
