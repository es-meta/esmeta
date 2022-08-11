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
  val help = "injects assertions to check the final state of a JavaScript file."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): String =
    val filename = getFirstFilename(globalConfig, this.name)
    val st = Initialize.fromFile(cfg, filename, test262 = false)
    val injected = Injector(st, assertions = config.assertions)
    for { path <- config.dumpOpt } dumpFile(injected, path)
    injected
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "assertions",
      BoolOption(c => c.assertions = true),
      "prepend helpers of assertions.",
    ),
    (
      "dump",
      StrOption((c, s) => c.dumpOpt = Some(s)),
      "dump an injected JavaScript program to a given path.",
    ),
  )
  case class Config(
    var assertions: Boolean = false,
    var dumpOpt: Option[String] = None,
  )
}
