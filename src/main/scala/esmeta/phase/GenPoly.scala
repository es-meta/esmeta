package esmeta.phase

import esmeta.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.polyfill.{PolyfillGenerator, PolyfillPackager}
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `gen-poly` phase */
case object GenPoly extends Phase[Spec, List[Polyfill]] {
  val name = "gen-poly"
  val help = "generates a polyfill library from ECMA-262."
  def apply(
    spec: Spec,
    cmdConfig: CommandConfig,
    config: Config,
  ): List[Polyfill] = {
    // The rules rewrite spec steps into internal operations; the optimized
    // runtime then implements those operations over data structures the rules
    // introduce, so asking for it turns the rules on as well. A custom rule
    // directory wins over the bundled one.
    val useDsl = config.dsl || config.opt || config.dslDir.isDefined
    val dslDir =
      if (useDsl) Some(config.dslDir.getOrElse(POLYFILL_RULES_DIR)) else None
    val polyfills = PolyfillGenerator(spec, dslDir)

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

    PolyfillPackager(
      polyfills,
      config.out.getOrElse(POLYFILL_OUT_DIR),
      config.targets,
      config.opt,
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
      "dsl",
      BoolOption(_.dsl = _),
      "rewrite specification steps using the bundled transformation rules.",
    ),
    (
      "opt",
      BoolOption(_.opt = _),
      "link the optimized runtime operations (implies -gen-poly:dsl).",
    ),
    (
      "target",
      StrOption((c, s) =>
        c.targets = s.split(",").map(_.trim).filter(_.nonEmpty).toList,
      ),
      "select built-ins to generate, as comma-separated globs (default: all).",
    ),
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      s"set the output directory (default: $POLYFILL_OUT_DIR).",
    ),
    (
      "dsl-dir",
      StrOption((c, s) => c.dslDir = Some(s)),
      "set a custom transformation rule directory (implies -gen-poly:dsl).",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var loc: Boolean = false,
    var dsl: Boolean = false,
    var opt: Boolean = false,
    var targets: List[String] = Nil,
    var out: Option[String] = None,
    var dslDir: Option[String] = None,
  )
}
