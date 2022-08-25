package esmeta.ai.repl.command

import esmeta.ai.*
import esmeta.ai.repl.*

// list-break command
case object CmdListBreak
  extends Command(
    "list-break",
    "Show the list of break points.",
  ) {
  // options
  val options: List[String] = Nil

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = for {
    ((k, v), i) <- repl.breakpoints.zipWithIndex
  } println(f"$i: $k%-15s $v")
}
