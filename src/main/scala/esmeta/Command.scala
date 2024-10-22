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

  /** help message */
  def examples: List[String]

  /** show the final result */
  def showResult(res: Result): Unit = println(res)

  /** target name */
  def targetName: String = ""

  /** need target */
  def needTarget: Boolean = targetName != ""

  /** run command with command-line arguments */
  def apply(args: List[String]): Result =
    val cmdConfig = CommandConfig(this)
    val parser = ArgParser(this, cmdConfig)
    val runner = pList.getRunner(parser)
    parser(args)
    ESMeta(this, runner(_), cmdConfig)

  /** run command with command-line arguments */
  def apply(args: String): Result = apply(args.split(" +").toList)

  /** a list of phases without specific IO types */
  def phases: Vector[Phase[_, _]] = pList.phases

  /** append a phase to create a new phase list */
  def >>[R](phase: Phase[Result, R]): PhaseList[R] = pList >> phase
}

/** base command */
case object CmdBase extends Command("", PhaseNil) {
  val help = "does nothing."
  val examples = Nil
}

/** `help` command */
case object CmdHelp extends Command("help", CmdBase >> Help) {
  val help = "shows help messages."
  val examples = List(
    "esmeta help                  // show help message.",
    "esmeta help extract          // show help message of `extract` command.",
  )
  override val targetName = "[<command>]"
  override val needTarget = false
}

// -----------------------------------------------------------------------------
// Mechanized Specification Extraction
// -----------------------------------------------------------------------------
/** `extract` command */
case object CmdExtract extends Command("extract", CmdBase >> Extract) {
  val help = "extracts specification model from ECMA-262 (spec.html)."
  val examples = List(
    "esmeta extract                           // extract current version.",
    "esmeta extract -extract:target=es2022    // extract es2022 version.",
    "esmeta extract -extract:target=868fe7a   // extract 868fe7a hash version.",
  )
}

/** `compile` command */
case object CmdCompile extends Command("compile", CmdExtract >> Compile) {
  val help = "compiles a specification to an IR program."
  val examples = List(
    "esmeta compile                        # compile spec to IR program.",
    "esmeta compile -extract:target=es2022 # compile es2022 spec to IR program",
  )
}

/** `build-cfg` command */
case object CmdBuildCFG extends Command("build-cfg", CmdCompile >> BuildCFG) {
  val help = "builds a control-flow graph (CFG) from an IR program."
  val examples = List(
    "esmeta build-cfg                          # build CFG for spec.",
    "esmeta build-cfg -extract:target=es2022   # build CFG for es2022 spec.",
  )
}

// -----------------------------------------------------------------------------
// Analysis of ECMA-262
// -----------------------------------------------------------------------------
/** `tycheck` command */
case object CmdTypeCheck extends Command("tycheck", CmdBuildCFG >> TypeCheck) {
  val help = "performs a type analysis of ECMA-262."
  val examples = List(
    "esmeta tycheck                              # type check for spec.",
    "esmeta tycheck -tycheck:target='.*ToString' # type check with targets",
    "esmeta tycheck -extract:target=es2022       # type check for es2022 spec.",
  )
}

// -----------------------------------------------------------------------------
// Interpreter & Double Debugger for ECMAScript
// -----------------------------------------------------------------------------
/** `parse` command */
case object CmdParse extends Command("parse", CmdExtract >> Parse) {
  val help = "parses an ECMAScript file."
  val examples = List(
    "esmeta parse a.js                         # parse a.js file.",
    "esmeta parse a.js -extract:target=es2022  # parse with es2022 spec.",
    "esmeta parse a.js -parse:debug            # parse in the debugging mode.",
  )
  override val targetName = "<js>+"
}

/** `eval` command */
case object CmdEval extends Command("eval", CmdBuildCFG >> Eval) {
  val help = "evaluates an ECMAScript file."
  val examples = List(
    "esmeta eval a.js                         # eval a.js file.",
    "esmeta eval a.js -extract:target=es2022  # eval with es2022 spec.",
    "esmeta eval a.js -eval:log               # eval in the logging mode.",
  )
  override val targetName = "<js>+"
}

/** `web` command */
case object CmdWeb extends Command("web", CmdBuildCFG >> Web) {
  val help = "starts a web server for an ECMAScript double debugger."
  val examples = List(
    "esmeta web    # turn on the server (Use with `esmeta-debugger-client`).",
  )
}

// -----------------------------------------------------------------------------
// Tester for Test262 (ECMAScript Test Suite)
// -----------------------------------------------------------------------------
/** `test262-test` command */
case object CmdTest262Test
  extends Command("test262-test", CmdBuildCFG >> Test262Test) {
  val help = "tests Test262 tests with harness files (default: tests/test262)."
  val examples = List(
    "esmeta test262-test                                           # all ",
    "esmeta test262-test tests/test262/test/built-ins/Map/map.js   # file",
    "esmeta test262-test tests/test262/test/language/expressions   # directory",
  )
  override val targetName = "<js|dir>+"
  override val needTarget = false
}

// -----------------------------------------------------------------------------
// ECMAScript Transformer
// -----------------------------------------------------------------------------
/** `inject` command */
case object CmdInject extends Command("inject", CmdBuildCFG >> Inject) {
  val help = "injects assertions to check final state of an ECMAScript file."
  val examples = List(
    "esmeta inject a.js                               # inject assertions.",
    "esmeta inject a.js -inject:defs -inject:out=b.js # dump with definitions.",
  )
  override val targetName = "<js>+"
}

/** `mutate` command */
case object CmdMutate extends Command("mutate", CmdBuildCFG >> Mutate) {
  def help = "mutates an ECMAScript program."
  val examples = List(
    "esmeta mutate a.js                           # mutate ECMAScript program.",
    "esmeta mutate a.js -mutate:out=b.js          # dump the mutated program.",
    "esmeta mutate a.js -mutate:mutator=random    # use random mutator.",
  )
}

// -----------------------------------------------------------------------------
// ECMAScript Static Analysis (Meta-Level Static Analysis)
// -----------------------------------------------------------------------------
/** `analyze` command */
case object CmdAnalyze extends Command("analyze", CmdBuildCFG >> Analyze) {
  val help = "analyzes an ECMAScript file using meta-level static analysis."
  val examples = List(
    "esmeta analyze a.js                         # analyze a.js file.",
    "esmeta analyze a.js -extract:target=es2022  # analyze with es2022 spec.",
    "esmeta analyze a.js -analyze:repl           # analyze in a REPL mode.",
  )
  override val targetName = "<js>+"
}

// -----------------------------------------------------------------------------
// IR-ES Interpreter
// -----------------------------------------------------------------------------
/** `ir-eval` command */
case object CmdIRInterp
  extends Command("ir-eval", CmdBase >> IRRead >> IRPEval >> IREval) {
  val help = "evaluate an IR-ES (ESMeta IR) file."
  val examples = List(
    "esmeta ir-eval a.ir                          # run a.ir (IR-ES) file.",
    "esmeta ir-eval a.ir -ir-peval:turn-on        # run a.ir (IR-ES) file with partial-evaluation.",
    "esmeta ir-eval a.ir -ir-eval:format          # run a.ir (IR-ES) file and format it in a cannonical form.",
  )
  override val targetName = "<ir>+"

}

// -----------------------------------------------------------------------------
// Partial Evaluator
// -----------------------------------------------------------------------------

/** `peval` command */
case object CmdPEval extends Command("peval", CmdCompile >> PEval) {
  val help =
    "(WIP) partial-evaluates 'FunctionDeclarationInstantiation' using an ECMAScript file."
  val examples = List(
    "esmeta peval a.js                         # partial-eval using a.js file.",
  )
  override val targetName = "<js>+"
}

/** `peval-then-eval` command */
case object CmdPThenEval extends Command("pte", CmdPEval >> BuildCFG >> Eval) {
  val help =
    "(WIP) peval-then-eval an ECMAScript file using overloaded `FunctionDeclarationInstantiation`."
  val examples = List(
    "esmeta pte a.js                         # peval-then-eval a.js file.",
    "esmeta pte a.js -extract:target=es2022  # peval-then-eval with es2022 spec.",
    "esmeta pte a.js -eval:log               # peval-then-eval in the logging mode.",
  )
  override val targetName = "<js>+"
}
