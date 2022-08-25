package esmeta.ai.repl.command

import esmeta.ai.*
import esmeta.ai.repl.*
import esmeta.util.BaseUtils.*

// rm-break command
case object CmdRmBreak
  extends Command(
    "rm-break",
    "Remove a break point.",
  ) {
  // options
  val options @ List(all) = List("all")

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = args match {
    case Nil => println("need arguments")
    case arg :: _ => {
      val breakpoints = repl.breakpoints
      optional(arg.toInt) match {
        case _ if arg == s"-$all" => breakpoints.clear
        case Some(idx) if idx.toInt < breakpoints.size =>
          breakpoints.remove(idx.toInt)
        case _ => println("Inappropriate argument")
      }
    }
  }
}
