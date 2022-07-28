package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `type-analyze` phase */
case object TypeAnalyze extends Phase[CFG, AbsSemantics] {
  val name = "type-analyze"
  val help = "performs a type analysis of ECMA-262"
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): AbsSemantics = {
    domain._cfgOpt = Some(cfg) // initalize global CFG
    // AbsSemantics(readFile(filename), None, config.execLevel).fixpoint
    domain.ABS_STATE = domain.TypeStateDomain
    domain.ABS_VALUE = domain.TypeDomain
    AbsSemantics.typeAnalysisTest().fixpoint
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "repl",
      BoolOption(c => USE_REPL = true),
      "use a REPL for meta-level static analysis.",
    ),
  )
  case class Config()
}
