package esmeta.phase

import esmeta.{LINE_SEP, Command}
import esmeta.util.*
import esmeta.{ESMeta, GlobalConfig}

/** `help` phase */
case object Help extends Phase[Unit, String]:
  val name = "help"
  val help = "shows help messages."
  def apply(
    unit: Unit,
    globalConfig: GlobalConfig,
    config: Config,
  ): String = globalConfig.args.headOption match
    case None => helpMessage
    case Some(name) =>
      ESMeta.cmdMap.get(name) match
        case Some(cmd) => cmdHelp(cmd)
        case None      => helpMessage

  def cmdHelp(cmd: Command[_]): String =
    val app = new Appender
    app >> "The command `" >> cmd.name >> "` " >> cmd.help
    app :> ""
    app :> "- Usage:"
    app :> "    esmeta " >> cmd.name >> " {option}* {filename}?"
    app :> ""
    app :> "- phase list: " >> s"(${cmd.pList})"
    app :> "    Each phase has following options."
    app :> "    format: {phase} [-{phase}:{option}[={input}]]*"
    for (phase <- cmd.phases)
      app :> ""
      header(app, phase.name)
      body(app, phase.help)
      app :> ""
      for ((name, desc) <- phase.getOptDescs)
        body(app, s"If $name is given, $desc", true)
    app.toString

  /* help message string */
  lazy val helpMessage =
    val app = new Appender
    app >> "- Usage:"
    app :> "    esmeta {command} {option}* {filename}?"
    app :> ""
    app >> "- command list:"
    app :> "    Each command consists of following phases."
    app :> "    format: {command} {phase} [>> {phase}]*"
    app :> ""
    for (cmd <- ESMeta.commands)
      header(app, cmd.name)
      body(app, cmd.help)
      body(app, s"(${cmd.pList})", true)
      app :> ""
    app :> "- phase list:"
    app :> "    Each phase has following options."
    app :> "    format: {phase} [-{phase}:{option}[={input}]]*"
    app :> ""
    for (phase <- ESMeta.phases)
      header(app, phase.name)
      body(app, phase.help)
      app :> ""
      for ((name, desc) <- phase.getOptDescs)
        body(app, s"If $name is given, $desc", true)
      app :> ""
    app :> "- global option:"
    app :> ""
    for ((opt, kind, desc) <- ESMeta.options)
      app :> "    If -" >> opt >> kind.postfix >> " is given, " >> desc
    app.toString

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
          width = 0
        else if (j != 0) app >> " "
        app >> w
        width += w.length + 1
    }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = Nil
  case class Config()
