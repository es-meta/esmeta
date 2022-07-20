package esmeta.analyzer.util.command

import esmeta.analyzer.*
import esmeta.analyzer.util.*

// continue command
case object CmdContinue
  extends Command(
    "continue",
    "Continue static analysis.",
  ) {
  // options
  val options: List[String] = Nil

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = repl.continue = true
}
