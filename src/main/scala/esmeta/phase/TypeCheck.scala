package esmeta.phase

import esmeta.*
import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.analyzer.{Config => AnalysisConfig, *}
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `type-check` phase */
case object TypeCheck extends Phase[CFG, AbsSemantics] {
  val name = "type-check"
  val help = "performs a type analysis of ECMA-262."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): AbsSemantics = TypeAnalyzer(cfg)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "repl",
      BoolOption(c => AnalysisConfig.USE_REPL = true),
      "use a REPL for type analysis of ECMA-262.",
    ),
  )
  case class Config()
}
