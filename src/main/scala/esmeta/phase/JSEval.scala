package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.util.SystemUtils.*
import esmeta.js.*

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
    val content = readFile(filename)
    val st = Initialize(cfg, content)
    Interp(st)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
