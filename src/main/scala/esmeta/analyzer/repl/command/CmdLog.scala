package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*

trait CmdLogDecl { self: Self =>

// log command
  case object CmdLog
    extends Command(
      "log",
      "Dump the current analysis result.",
    ) {
    // options
    val options = Nil

    // run command
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit = logging
  }
}
