package esmeta.analyzer.util.command

import esmeta.analyzer.*
import esmeta.analyzer.util.*
import esmeta.ANALYZE_LOG_DIR

// graph command
case object CmdGraph
  extends Command(
    "graph",
    "Dump the current control graph.",
  ) {
  // options
  // val options @ List(total) = List("total")
  val options = Nil

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = for { cp <- cpOpt } {
    cp.func.dumpDot(ANALYZE_LOG_DIR, filenameOpt = Some("repl"))
  }
}
