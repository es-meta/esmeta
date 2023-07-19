package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*

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
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = for {
    ((k, v), i) <- REPL.breakpoints.zipWithIndex
  } println(f"$i: $k%-15s $v")
}
