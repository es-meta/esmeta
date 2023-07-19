package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.util.BaseUtils.*

// jump command
case object CmdJump
  extends Command(
    "jump",
    "Jump to a specific iteration.",
  ) {
  // options
  val options @ List(entry, merged) = List("entry", "merged")

  // run command
  def apply(
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = args match {
    case s"-${`entry`}" :: _ =>
      REPL.nextEntry = true; REPL.continue = true
    case s"-${`merged`}" :: _ =>
      REPL.untilMerged = true; REPL.continue = true
    case arg :: _ if !optional(arg.toInt).isEmpty =>
      val iter = arg.toInt
      if (iter > REPL.iter) { REPL.jumpTo = Some(iter); REPL.continue = true }
      else println(s"The iteration [$iter] is already passed.")
    case _ => println("Inappropriate argument")
  }
}
