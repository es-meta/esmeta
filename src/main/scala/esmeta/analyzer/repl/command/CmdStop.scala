package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.util.BaseUtils.*

trait CmdStopDecl { self: Self =>

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
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit = Repl.stop
  }
}
