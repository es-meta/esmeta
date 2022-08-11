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
  val help = "analyzes a JavaScript file using meta-level static analysis."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): AbsSemantics = {
    // initialize
    domain._cfgOpt = Some(cfg)
    val filename = getFirstFilename(globalConfig, this.name)
    val npMap = Initialize.initJs(readFile(filename))

    // perform a meta-level static analysis
    AbsSemantics(npMap, config.execLevel).fixpoint
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "repl",
      BoolOption(c => USE_REPL = true),
      "use a REPL for meta-level static analysis.",
    ),
    (
      "exec-level",
      NumOption((c, k) => {
        if (k < 0 || k > 2) println(s"Invalid execution level: $k")
        c.execLevel = k
      }),
      "use concrete execution to check soundness.",
    ),
  )
  case class Config(var execLevel: Int = 0)
}
