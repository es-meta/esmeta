package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `analyze` phase */
case object Analyze extends Phase[CFG, ESAnalyzer#Semantics] {
  val name = "analyze"
  val help = "analyzes an ECMAScript file using meta-level static analysis."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): ESAnalyzer#Semantics =
    val filename = getFirstFilename(cmdConfig, this.name)
    val sourceText = readFile(filename).trim
    ESAnalyzer(cfg, config.useRepl)(sourceText)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "repl",
      BoolOption(_.useRepl = _),
      "use a REPL for meta-level static analysis.",
    ),
  )
  case class Config(
    var useRepl: Boolean = false,
  )
}
