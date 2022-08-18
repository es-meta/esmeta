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
    val cmdConfig = CommandConfig(this)
    val parser = ArgParser(this, cmdConfig)
    val runner = pList.getRunner(parser)
    parser(args)
    ESMeta(this, runner(_), cmdConfig)

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
/** `type-check` command */
case object CmdTypeCheck
  extends Command("type-check", CmdBuildCFG >> TypeCheck) {
  val help = "performs a type analysis of ECMA-262."
}

// -----------------------------------------------------------------------------
// Interpreter & Double Debugger for ECMAScript
// -----------------------------------------------------------------------------
/** `parse` command */
case object CmdParse extends Command("parse", CmdExtract >> Parse) {
  val help = "parses an ECMAScript file."
}

/** `eval` command */
case object CmdEval extends Command("eval", CmdBuildCFG >> Eval) {
  val help = "evaluates an ECMAScript file."
}

/** `web` command */
case object CmdWeb extends Command("web", CmdBuildCFG >> Web) {
  val help = "starts a web server for an ECMAScript double debugger."
}

// -----------------------------------------------------------------------------
// Tester for Test262 (ECMAScript Test Suite)
// -----------------------------------------------------------------------------
/** `test262-test` command */
case object CmdTest262Test
  extends Command("test262-test", CmdBuildCFG >> Test262Test) {
  val help = "test Test262 tests with harness files (default: tests/test262)."
}

// -----------------------------------------------------------------------------
// ECMAScript Transformer
// -----------------------------------------------------------------------------
/** `inject` command */
case object CmdInject extends Command("inject", CmdBuildCFG >> Inject) {
  val help = "injects assertions to check final state of an ECMAScript file."
}

// -----------------------------------------------------------------------------
// ECMAScript Static Analysis (Meta-Level Static Analysis)
// -----------------------------------------------------------------------------
/** `analyze` command */
case object CmdAnalyze extends Command("analyze", CmdBuildCFG >> Analyze) {
  val help = "analyzes an ECMAScript file using meta-level static analysis."
}
