package esmeta

import esmeta.phase.*
import esmeta.util.ArgParser

/** commands
  *
  * @tparam Result
  *   the result typeof command
  */
sealed abstract class Command[Result](
  /** command name */
  val name: String,

  /** phase list */
  val pList: PhaseList[Result],
) {
  override def toString: String = pList.toString

  /** help message */
  def help: String

  /** show the final result */
  def showResult(res: Result): Unit = ()

  /** run command with command-line arguments */
  def apply(args: List[String]): Result =
    val esmetaConfig = ESMetaConfig(this)
    val parser = new ArgParser(this, esmetaConfig)
    val runner = pList.getRunner(parser)
    parser(args)
    ESMeta(this, runner(_), esmetaConfig)

  /** append a phase to create a new phase list */
  def >>[R](phase: Phase[Result, R]): PhaseList[R] = pList >> phase
}

/** base command */
case object CmdBase extends Command("", PhaseNil) {
  def help = "does nothing."
}

/** `help` command */
case object CmdHelp extends Command("help", CmdBase >> Help) {
  def help = "shows help messages."
}
