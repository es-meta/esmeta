package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.util.BaseUtils.*

trait CmdJumpDecl { self: Self =>

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
        Repl.nextEntry = true; Repl.continue = true
      case s"-${`merged`}" :: _ =>
        Repl.untilMerged = true; Repl.continue = true
      case arg :: _ if !optional(arg.toInt).isEmpty =>
        val iter = arg.toInt
        if (iter > Repl.iter) { Repl.jumpTo = Some(iter); Repl.continue = true }
        else println(s"The iteration [$iter] is already passed.")
      case _ => println("Inappropriate argument")
    }
  }
}
