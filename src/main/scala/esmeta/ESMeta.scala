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
          case Some(CmdHelp) => println(ESMeta.help)
          case Some(cmd)     => cmd(args)
          case None          => throw NoCmdError(str)
        }
      case Nil => throw NoInputError
  catch
    // ESMetaError: print the error message.
    case ex: ESMetaError if !DEBUG =>
      Console.err.println(ex.getMessage)
    // Unexpected: print the stack trace.
    case ex: Throwable =>
      Console.err.println("* Unexpected error occurred.")
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
    CmdExtract,
    CmdCompile,
    CmdBuildCFG,
    CmdIREval,
    CmdJSParse,
    CmdJSEval,
    CmdFilterTest262,
  )
  val cmdMap = commands.foldLeft[Map[String, Command[_]]](Map()) {
    case (map, cmd) => map + (cmd.name -> cmd)
  }

  /** phases */
  var phases: List[Phase[_, _]] = List(
    Help,
    Extract,
    Compile,
    BuildCFG,
    IREval,
    JSParse,
    JSEval,
    FilterTest262,
  )

  /** global options */
  val options: List[PhaseOption[GlobalConfig]] = List(
    ("silent", BoolOption(c => SILENT = true), "do not show final results."),
    ("debug", BoolOption(c => DEBUG = true), "turn on the debug mode."),
    ("log", BoolOption(c => LOG = true), "turn on the logging mode."),
    ("time", BoolOption(c => TIME = true), "display the duration time."),
  )

  /* left align with a fixed width */
  private val WIDTH = 20
  private def align(str: String): String = s"%-${WIDTH}s".format(str)

  /** show help message */
  val help: String =
    val app = new Appender
    app >> "* command list:"
    app :> "    Each command consists of following phases."
    app :> "    format: {command} {phase} [>> {phase}]*"
    app :> ""
    for (cmd <- commands)
      app :> "    " >> align(cmd.name) >> cmd.help
      app :> "    " >> align("") >> "(" >> cmd.pList.toString >> ")"
    app :> ""
    app :> "* phase list:"
    app :> "    Each phase has following options."
    app :> "    format: {phase} [-{phase}:{option}[={input}]]*"
    app :> ""
    for (phase <- phases)
      app :> "    " >> align(phase.name) >> phase.help
      app :> ""
      for ((name, desc) <- phase.getOptDescs)
        app :> "    " >> align("") >> "If " >> name >> " is given, " >> desc
      app :> ""
    app :> "* global option:"
    app :> ""
    for ((opt, kind, desc) <- options)
      app :> "    If -" >> opt >> kind.postfix >> " is given, " >> desc
    app.toString
}

/** global configuration */
case class GlobalConfig(
  var command: Command[_],
  var args: List[String] = Nil,
)
