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
    val parser = ArgParser(this, globalConfig)
    val runner = pList.getRunner(parser)
    parser(args)
    ESMeta(this, runner(_), globalConfig)

  /** a list of phases without specific IO types */
  def phases: Vector[Phase[_, _]] = pList.phases

  /** append a phase to create a new phase list */
  def >>[R](phase: Phase[Result, R]): PhaseList[R] = pList >> phase
}

/** base command */
case object CmdBase extends Command("", PhaseNil) {
  val help = "does nothing."
}

/** `help` command */
case object CmdHelp extends Command("help", CmdBase >> Help) {
  val help = "shows help messages."
}

// -----------------------------------------------------------------------------
// Mechanized Specification Extraction
// -----------------------------------------------------------------------------
/** `extract` command */
case object CmdExtract extends Command("extract", CmdBase >> Extract) {
  val help = "extracts specification model from ECMA-262 (spec.html)."
}

/** `compile` command */
case object CmdCompile extends Command("compile", CmdExtract >> Compile) {
  val help = "compiles a specification to an IR program."
}

/** `build-cfg` command */
case object CmdBuildCFG extends Command("build-cfg", CmdCompile >> BuildCFG) {
  val help = "builds a control-flow graph (CFG) from an IR program."
}

// -----------------------------------------------------------------------------
// Analysis of ECMA-262
// -----------------------------------------------------------------------------
/** `typecheck` command */
case object CmdTypeCheck
  extends Command("typecheck", CmdBuildCFG >> TypeCheck) {
  val help = "performs a type analysis of ECMA-262."
}

// -----------------------------------------------------------------------------
// JavaScript Interpreter
// -----------------------------------------------------------------------------
/** `parse` command */
case object CmdParse extends Command("parse", CmdExtract >> Parse) {
  val help = "parses a JavaScript file."
}

/** `eval` command */
case object CmdEval extends Command("eval", CmdBuildCFG >> Eval) {
  val help = "evaluates a JavaScript file."
}

/** `test262test` command */
case object CmdTest262Test
  extends Command("test262test", CmdBuildCFG >> Test262Test) {
  val help = "test a Test262 program with harness files."
  override def showResult(msg: Option[String]): Unit = msg match
    case None      => println(s"PASS")
    case Some(msg) => println(s"FAIL - $msg")
}

/** `web` command */
case object CmdWeb extends Command("web", CmdBuildCFG >> Web) {
  val help = "starts a web server for a JavaScript double debugger."
}

// -----------------------------------------------------------------------------
// JavaScript Transformer
// -----------------------------------------------------------------------------
/** `inject` command */
case object CmdInject extends Command("inject", CmdBuildCFG >> Inject) {
  val help = "injects assertions to check the final state of a JavaScript file."
}

// -----------------------------------------------------------------------------
// JavaScript Static Analysis (Meta-Level Static Analysis)
// -----------------------------------------------------------------------------
/** `analyze` command */
case object CmdAnalyze extends Command("analyze", CmdBuildCFG >> Analyze) {
  val help = "analyzes a JavaScript file using meta-level static analysis."
}
