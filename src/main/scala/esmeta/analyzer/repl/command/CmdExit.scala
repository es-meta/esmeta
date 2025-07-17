package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.util.BaseUtils.*

trait CmdExitDecl { self: Self =>

// exit command
  case object CmdExit
    extends Command(
      "exit",
      "Exit the type checking.",
    ) {
    // options
    val options = Nil

    // run command
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit = raise("stop for debugging")
  }
}
