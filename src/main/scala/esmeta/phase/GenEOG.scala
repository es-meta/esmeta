package esmeta.phase

import esmeta.*
import esmeta.analyzer.eoggen.*
import esmeta.cfg.{CFG, Func}
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `gen-eog` phase */
case object GenEOG extends Phase[CFG, Unit] {
  val name = "gen-eog"
  val help = "generates and execution-order graph (EOG)."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val filename = getFirstFilename(cmdConfig, name)
    val ast = cfg.scriptParser.fromFile(filename)
    val analyzer = EOGGenerator(
      cfg = cfg,
      ast = ast,
      log = config.log,
      useRepl = config.useRepl,
    )
    analyzer.analyze

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "repl",
      BoolOption(_.useRepl = _),
      "use a REPL for type checking of ECMA-262.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "logging mode.",
    ),
  )
  case class Config(
    var useRepl: Boolean = false,
    var log: Boolean = false,
  )
}
