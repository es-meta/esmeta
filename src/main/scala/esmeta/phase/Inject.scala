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
    val injected = Injector(st, config.defs)
    // dump the assertion-injected JS program
    for (filename <- config.out)
      dumpFile(
        name = "an assertion-injected JavaScript program",
        data = injected,
        filename = filename,
      )
    injected
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "defs",
      BoolOption(c => c.defs = true),
      "prepend definitions of helpers for assertions.",
    ),
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump an assertion-injected JavaScript program to a given path.",
    ),
  )
  case class Config(
    var defs: Boolean = false,
    var out: Option[String] = None,
  )
}
