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

/** `compile` command */
case object CmdCompile extends Command("compile", CmdExtract >> Compile) {
  def help = "compiles a specification to an IR program."
}

/** `build-cfg` command */
case object CmdBuildCFG extends Command("build-cfg", CmdCompile >> BuildCFG) {
  def help = "builds a control-flow graph (CFG) from an IR program."
}

/** `parse` command */
case object CmdParse extends Command("parse", CmdBuildCFG >> Parse) {
  def help = "parses a JavaScript file."
}

/** `eval` command */
case object CmdEval extends Command("eval", CmdBuildCFG >> Eval) {
  def help = "evaluates a JavaScript file."
}

/** `test262test` command */
case object CmdTest262Test
  extends Command("test262test", CmdBuildCFG >> Test262Test) {
  def help = "test a Test262 program with harness files."
  override def showResult(msg: Option[String]): Unit = msg match
    case None      => println(s"PASS")
    case Some(msg) => println(s"FAIL - $msg")
}

/** `inject` command */
case object CmdInject extends Command("inject", CmdBuildCFG >> Inject) {
  def help =
    "injects assertions to check the final state of a given JavaScript program."
}

/** `web` command */
case object CmdWeb extends Command("web", CmdBuildCFG >> Web) {
  def help = "starts a web server for interactive execution."
}
