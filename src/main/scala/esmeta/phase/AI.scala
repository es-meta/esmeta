package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.ai.{Config => AnalysisConfig, *}
import esmeta.ai.domain
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `ai` phase */
case object AI extends Phase[CFG, AbsSemantics] {
  val name = "ai"
  val help = "analyzes an ECMAScript file using meta-level static analysis."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): AbsSemantics =
    AnalysisConfig.YET_THROW = true
    val filename = getFirstFilename(cmdConfig, this.name)
    val analyzer = ESAnalyzer(cfg)
    analyzer(readFile(filename).trim)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "repl",
      BoolOption(c => AnalysisConfig.USE_REPL = true),
      "use a REPL for meta-level static analysis.",
    ),
  )
  case class Config()
}
