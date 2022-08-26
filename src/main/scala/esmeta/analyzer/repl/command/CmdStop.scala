package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.util.BaseUtils.*

// stop command
case object CmdStop
  extends Command(
    "stop",
    "Stop the repl.",
  ) {
  // options
  val options = Nil

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = repl.stop
}
