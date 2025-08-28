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
    val polyfills = PolyfillGenerator(spec)

    // logging mode
    if (config.log)
      dumpDir(
        name = "generated polyfills",
        iterable = ProgressBar("Dump polyfills", polyfills, detail = false),
        dirname = POLYFILL_LOG_DIR,
        getName = poly => s"${poly.belongsTo}/${poly.name}.js",
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
      "log-with-loc",
      BoolOption((c, b) => { c.log ||= b; c.loc = b }),
      "turn on logging mode with location info.",
    ),
    (
      "opt",
      BoolOption((c, b) => { c.opt ||= b; c.opt = b }),
      "turn on ir optimization",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var loc: Boolean = false,
    var opt: Boolean = false,
  )
}
