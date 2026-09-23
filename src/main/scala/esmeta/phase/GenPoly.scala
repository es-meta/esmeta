package esmeta.phase

import esmeta.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.polyfill.{Generator, Packager, Polyfill}
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
    // The rules rewrite spec steps into internal operations, and the optimized
    // runtime implements those operations over the data structures the rules
    // introduce: neither half is correct without the other, so they are a
    // single choice rather than two flags. A custom rule directory selects
    // which rules to apply and implies that choice.
    val optimize = config.opt || config.dslDir.isDefined
    val dslDir =
      if (optimize) Some(config.dslDir.getOrElse(POLYFILL_RULES_DIR)) else None
    val polyfills = Generator(spec, dslDir)

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

    Packager(
      polyfills,
      config.out.getOrElse(POLYFILL_OUT_DIR),
      config.targets,
      optimize,
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
      "opt",
      BoolOption(_.opt = _),
      "rewrite specification steps with the bundled transformation rules and " +
      "link the optimized runtime operations they target.",
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
      "set a custom transformation rule directory (implies -gen-poly:opt).",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var loc: Boolean = false,
    var opt: Boolean = false,
    var targets: List[String] = Nil,
    var out: Option[String] = None,
    var dslDir: Option[String] = None,
  )
}
