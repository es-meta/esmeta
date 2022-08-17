package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `analyze` phase */
case object Analyze extends Phase[CFG, AbsSemantics] {
  val name = "analyze"
  val help = "analyzes an ECMAScript file using meta-level static analysis."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): AbsSemantics = {
    // initialize
    domain._cfgOpt = Some(cfg)
    YET_THROW = true
    val filename = getFirstFilename(cmdConfig, this.name)
    val npMap = Initialize.initEs(readFile(filename))

    // perform a meta-level static analysis
    AbsSemantics(npMap).fixpoint
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
