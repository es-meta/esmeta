package esmeta.ai.repl.command

import esmeta.ai.*
import esmeta.ai.repl.*

// commands
abstract class Command(
  // command name
  val name: String,

  // command help message
  val help: String = "",
) {
  // options
  val options: List[String]

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit

  // not yet supported message
  def notYetCmd: Unit =
    notYet("this command is not yet supported")
  def notYet(msg: String): Unit =
    println(s"[NotSupported] $msg @ $name")
}
object Command {
  val commands: List[Command] = List(
    CmdHelp,
    CmdContinue,
    CmdMove,
    CmdBreak,
    CmdListBreak,
    CmdRmBreak,
    CmdJump,
    CmdPrint,
    CmdLog,
    CmdGraph,
    CmdExit,
    CmdStop,
    CmdInfo,
    CmdEntry,
    CmdWorklist,
    CmdFindMerged,
  )
  val cmdMap: Map[String, Command] = commands.map(cmd => (cmd.name, cmd)).toMap
}
