package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.js.*

/** `eval` phase */
case object Eval extends Phase[CFG, State] {
  val name = "eval"
  val help = "evaluates a JavaScript file."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): State =
    val filename = getFirstFilename(globalConfig, this.name)
    val initSt = Initialize.fromFile(cfg, filename)
    val st = Interp(initSt, timeLimit = None)
    st
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
  )
  case class Config()
}
