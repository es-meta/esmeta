package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.js.*
import esmeta.js.util.*
import esmeta.test262.*

/** `inject` phase */
case object Inject extends Phase[CFG, String] {
  val name = "inject"
  val help = "injects assertions to check the final state of given program."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): String =
    val filename = getFirstFilename(globalConfig, this.name)
    val st = Initialize.fromFile(cfg, filename, test262 = false)
    Injector(st)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
