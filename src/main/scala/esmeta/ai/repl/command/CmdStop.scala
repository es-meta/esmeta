package esmeta.ai.repl.command

import esmeta.ai.*
import esmeta.ai.repl.*
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
