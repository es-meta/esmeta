package esmeta.phase

import esmeta.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `gen-poly` phase */
case object GenPoly extends Phase[Spec, List[Polyfill]] {
  val name = "gen-poly"
  val help = "generates polyfill code."
  def apply(
    spec: Spec,
    cmdConfig: CommandConfig,
    config: Config,
  ): List[Polyfill] = {
    val polyfills = PolyfillGenerator(spec, config.dslDir)

    // logging mode
    if (config.log)
      rmdir(POLYFILL_LOG_DIR)
      dumpDir(
        name = "generated polyfills",
        iterable = ProgressBar("Dump polyfills", polyfills, detail = false),
        dirname = POLYFILL_LOG_DIR,
        getName = poly =>
          if (poly.name.startsWith("INTRINSICS.yet:"))
            s"${poly.name.stripPrefix("INTRINSICS.yet:").replace("`", "").replace(".", "")}.poly"
          else s"${poly.name}.poly",
        getData = _.toString,
      )

    polyfills
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "dsl-dir",
      StrOption((c, s) => c.dslDir = Some(s)),
      "set a directory of custom transformation before polyfill extraction (default: none).",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var loc: Boolean = false,
    var dslDir: Option[String] = None,
  )
}
