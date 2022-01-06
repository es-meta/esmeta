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
  def showResult(res: Result): Unit = println(res)

  /** run command with command-line arguments */
  def apply(args: List[String]): Result =
    val globalConfig = GlobalConfig(this)
    val parser = new ArgParser(this, globalConfig)
    val runner = pList.getRunner(parser)
    parser(args)
    ESMeta(this, runner(_), globalConfig)

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

/** `extract` command */
case object CmdExtract extends Command("extract", CmdBase >> Extract) {
  def help = "extracts specification model from ECMA-262 (spec.html)."
}
