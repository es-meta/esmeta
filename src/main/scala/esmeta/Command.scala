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
case object CmdTyCheck extends Command("tycheck", CmdBuildCFG >> TyCheck) {
  val help = "performs a type checking of ECMA-262."
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

/** `transpile` command */
case object CmdTranspile
  extends Command("transpile", CmdBuildCFG >> Transpile) {
  val help = "parses an ECMAScript file."
  val examples = List(
    "esmeta transpile a.js                         # parse a.js file.",
    "esmeta transpile a.js -extract:target=es2022  # parse with es2022 spec.",
    "esmeta transpile a.js -parse:debug            # parse in the debugging mode.",
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
// ECMAScript Fuzzer
// -----------------------------------------------------------------------------
/** `fuzz` command */
case object CmdFuzz extends Command("fuzz", CmdBuildCFG >> Fuzz) {
  val help = "generate ECMAScript programs for fuzzing."
  val examples = List(
    "esmeta fuzz                 # generate ECMAScript programs for fuzzing",
    "esmeta fuzz -fuzz:log       # fuzz in the logging mode.",
  )
}

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

/** `dump-debugger` command */
case object CmdDumpDebugger
  extends Command("dump-debugger", CmdBuildCFG >> DumpDebugger) {
  def help =
    "dumps the resources required by the standalone debugger. (for internal use)"
  val examples = List(
    "esmeta dump-debugger                         # dump data to data.json",
  )
}

/** `dump-visualizer` command */
case object CmdDumpVisualizer
  extends Command("dump-visualizer", CmdBuildCFG >> DumpVisualizer) {
  def help =
    "dumps the resources required by the visualizer. (for internal use)"
  val examples = List(
    "esmeta dump-visualizer                      # dump resources for visualizer",
  )
}
