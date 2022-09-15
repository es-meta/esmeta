package esmeta

import esmeta.error.*
import esmeta.phase.*
import esmeta.util.*

/** ESMeta top-level object */
object ESMeta extends Git(BASE_DIR) {

  /** the main entry point of ESMeta. */
  def main(tokens: Array[String]): Unit = try
    tokens.toList match
      case str :: args =>
        cmdMap.get(str) match {
          case Some(cmd) => cmd(args)
          case None      => throw NoCmdError(str)
        }
      case Nil => println(welcome)
  catch
    // ESMetaError: print only the error message.
    case e: ESMetaError =>
      Console.err.println(e.getMessage)
      if (ERROR_MODE) throw e
    // Unexpected: print the stack trace.
    case e: Throwable =>
      Console.err.println(s"[ESMeta v$VERSION] Unexpected error occurred:")
      throw e

  /** execute ESMeta with a runner */
  def apply[Result](
    command: Command[Result],
    runner: CommandConfig => Result,
    config: CommandConfig,
  ): Result =
    // silent for help command
    if (command == CmdHelp) config.silent = true
    // target existence check
    if (command.needTarget && config.targets.isEmpty)
      throw NoTargetError(command)
    // set the start time.
    val startTime = System.currentTimeMillis
    // execute the command.
    val result: Result = runner(config)
    // duration
    val duration = Time(System.currentTimeMillis - startTime)
    // display the result.
    if (!config.silent) command.showResult(result)
    // display the time.
    if (config.time)
      val name = config.command.name
      println(f"The command '$name' took $duration.")
    // return result
    result

  /** welcome message */
  val welcome: String =
    s"""Welcome to ESMeta v$VERSION - ECMAScript Specification Metalanguage.
       |Please type `esmeta help` to see the help message.""".stripMargin

  /** commands */
  val commands: List[Command[_]] = List(
    CmdHelp,
    // Mechanized Specification Extraction
    CmdExtract,
    CmdCompile,
    CmdBuildCFG,
    // Analysis of ECMA-262
    CmdTypeCheck,
    // Interpreter & Double Debugger for ECMAScript
    CmdParse,
    CmdEval,
    CmdWeb,
    // Tester for Test262 (ECMAScript Test Suite)
    CmdTest262Test,
    // ECMAScript Transformer
    CmdInject,
    CmdMutate,
    // ECMAScript Static Analysis (Meta-Level Static Analysis)
    CmdAnalyze,
  )
  val cmdMap = commands.foldLeft[Map[String, Command[_]]](Map()) {
    case (map, cmd) => map + (cmd.name -> cmd)
  }

  /** phases */
  var phases: List[Phase[_, _]] = List(
    Help,
    // Mechanized Specification Extraction
    Extract,
    Compile,
    BuildCFG,
    // Analysis of ECMA-262
    TypeCheck,
    // Interpreter & Double Debugger for ECMAScript
    Parse,
    Eval,
    Web,
    // Tester for Test262 (ECMAScript Test Suite)
    Test262Test,
    // ECMAScript Transformer
    Inject,
    Mutate,
    // ECMAScript Static Analysis (Meta-Level Static Analysis)
    Analyze,
  )

  /** command options */
  val options: List[PhaseOption[CommandConfig]] = List(
    ("silent", BoolOption(c => c.silent = true), "do not show final results."),
    ("error", BoolOption(_ => ERROR_MODE = true), "show error stack traces."),
    ("time", BoolOption(c => c.time = true), "display the duration time."),
  )
}

/** command configuration */
case class CommandConfig(
  var command: Command[_],
  var targets: List[String] = Nil,
  var silent: Boolean = false,
  var time: Boolean = false,
)
