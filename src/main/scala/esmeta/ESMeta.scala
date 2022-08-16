package esmeta

import esmeta.error.*
import esmeta.phase.*
import esmeta.util.*

/** ESMeta top-level object */
object ESMeta {

  /** the main entry point of ESMeta. */
  def main(tokens: Array[String]): Unit = try
    tokens.toList match
      case str :: args =>
        cmdMap.get(str) match {
          case Some(cmd) => cmd(args)
          case None      => throw NoCmdError(str)
        }
      case Nil => throw NoInputError
  catch
    // ESMetaError: print the error message.
    case ex: ESMetaError if !DEBUG =>
      Console.err.println(ex.getMessage)
    // Unexpected: print the stack trace.
    case ex: Throwable =>
      Console.err.println("- Unexpected error occurred.")
      Console.err.println(ex.toString)
      Console.err.println(ex.getStackTrace.mkString(LINE_SEP))

  /** execute ESMeta with a runner */
  def apply[Result](
    command: Command[Result],
    runner: GlobalConfig => Result,
    config: GlobalConfig,
  ): Result =
    // set the start time.
    val startTime = System.currentTimeMillis
    // execute the command.
    val result: Result = runner(config)
    // duration
    val duration = System.currentTimeMillis - startTime
    // display the result.
    if (!SILENT) command.showResult(result)
    // display the time.
    if (TIME)
      val name = config.command.name
      println(s"The command '$name' took $duration ms.")
    // return result
    result

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
    CmdTest262Test,
    CmdWeb,
    // ECMAScript Transformer
    CmdInject,
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
    Test262Test,
    Web,
    // ECMAScript Transformer
    Inject,
    // ECMAScript Static Analysis (Meta-Level Static Analysis)
    Analyze,
  )

  /** global options */
  val options: List[PhaseOption[GlobalConfig]] = List(
    ("silent", BoolOption(c => SILENT = true), "do not show final results."),
    ("debug", BoolOption(c => DEBUG = true), "turn on the debugging mode."),
    ("time", BoolOption(c => TIME = true), "display the duration time."),
  )
}

/** global configuration */
case class GlobalConfig(
  var command: Command[_],
  var args: List[String] = Nil,
)
