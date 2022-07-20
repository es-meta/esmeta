package esmeta.analyzer.util.command

import esmeta.analyzer.*
import esmeta.analyzer.util.*
import esmeta.util.BaseUtils.*

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
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = error("stop for debugging")
}
