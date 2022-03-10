package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.js.*
import esmeta.test262.*

/** `js-eval` phase */
case object JSEval extends Phase[CFG, State] {
  val name = "js-eval"
  val help = "evaluates a JavaScript file."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): State =
    val filename = getFirstFilename(globalConfig, this.name)
    val st = Initialize.fromFile(cfg, filename, config.test262)
    Interp(st, timeLimit = None)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "test262",
      BoolOption(c => c.test262 = true),
      "prepend test262 harness files based on metadata.",
    ),
  )
  case class Config(
    var test262: Boolean = false,
  )
}
