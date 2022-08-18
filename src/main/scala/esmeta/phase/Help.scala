package esmeta.phase

import esmeta.{LINE_SEP, Command, VERSION}
import esmeta.util.*
import esmeta.{ESMeta, CommandConfig}

/** `help` phase */
case object Help extends Phase[Unit, Unit]:
  val name = "help"
  val help = "shows help messages."
  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = println(cmdConfig.targets.headOption match
    case None => helpMessage
    case Some(name) =>
      ESMeta.cmdMap.get(name) match
        case Some(cmd) => cmdHelp(cmd)
        case None      => helpMessage,
  )

  def cmdHelp(cmd: Command[_]): String =
    val app = Appender()
    app >> "The command `" >> cmd.name >> "` " >> cmd.help
    app :> ""
    app :> "Usage: esmeta " >> cmd.name >> " <option>*"
    if (cmd.targetName != "") app >> " " >> cmd.targetName
    app :> ""
    app :> "examples:"
    for (example <- cmd.examples) { app :> "  $ " >> example }
    app :> ""
    addPhase(app, cmd.phases)
    addGlobalOption(app)
    app.toString

  /* help message string */
  lazy val helpMessage =
    val app = Appender()
    app >> "ESMeta v" >> VERSION >> " - ECMAScript Specification Metalanguage"
    app :> ""
    app :> "Usage: esmeta <command> <option>* <filename>*"
    app :> ""
    app :> "If you want to see the detailed usage of each command,"
    app :> "please type `esmeta help <command>`."
    app :> ""
    app :> "- command list:"
    app :> "    Each command consists of following phases."
    app :> "    format: <command> <phase> [>> {phase}]*"
    app :> ""
    for (cmd <- ESMeta.commands)
      header(app, cmd.name)
      body(app, cmd.help)
      body(app, s"(${cmd.pList})", true)
      app :> ""

    addPhase(app, ESMeta.phases)
    addGlobalOption(app)
    app.toString

  def addPhase(app: Appender, phases: Iterable[Phase[_, _]]): Unit =
    app :> "- phase list:"
    app :> "    Each phase has following options."
    app :> "    format: <phase> [-<phase>:<option>[=<input>]]*"
    app :> ""
    for (phase <- phases)
      header(app, phase.name)
      body(app, phase.help)
      app :> ""
      for ((name, desc) <- phase.getOptDescs)
        body(app, s"If $name is given, $desc", true)
      app :> ""

  def addGlobalOption(app: Appender): Unit =
    app :> "- global option:"
    for ((opt, kind, desc) <- ESMeta.options)
      app :> "    If -" >> opt >> kind.postfix >> " is given, " >> desc

  /* constants */
  private val INDENT = 4
  private val HEADER_WIDTH = 20
  private val MAX_WIDTH = 100

  /* helper functions */
  private val pre = " " * INDENT
  private def header(app: Appender, str: String = ""): Unit =
    app :> pre >> s"%-${HEADER_WIDTH}s".format(str)
  private def body(
    app: Appender,
    str: String,
    firstIndent: Boolean = false,
  ): Unit =
    for ((l, i) <- str.split(LINE_SEP).zipWithIndex) {
      if (firstIndent || i != 0) header(app)
      var width = INDENT + HEADER_WIDTH
      for ((w, j) <- l.split(' ').zipWithIndex)
        if (width + w.length + 1 > MAX_WIDTH)
          header(app)
          app >> pre
          width = INDENT
        else if (j != 0) app >> " "
        app >> w
        width += w.length + 1
    }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = Nil
  case class Config()
