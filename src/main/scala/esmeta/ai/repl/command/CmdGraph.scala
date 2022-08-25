package esmeta.ai.repl.command

import esmeta.ai.*
import esmeta.ai.repl.*
import esmeta.util.SystemUtils.*
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
    mkdir(ANALYZE_LOG_DIR)
    cp.func.dumpDot(s"$ANALYZE_LOG_DIR/repl")
  }
}
