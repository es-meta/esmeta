package esmeta.phase

import esmeta.*
import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.cfg.{CFG, Func}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `tycheck` phase */
case object TypeCheck extends Phase[CFG, AbsSemantics] {
  val name = "tycheck"
  val help = "performs a type analysis of ECMA-262."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): AbsSemantics = TypeAnalyzer(
    cfg = cfg,
    target = config.target,
    strict = config.strict,
    ignore = config.ignore,
    log = config.log,
  )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target of type analysis with a regular expression pattern.",
    ),
    (
      "repl",
      BoolOption(c => USE_REPL = true),
      "use a REPL for type analysis of ECMA-262.",
    ),
    (
      "strict",
      BoolOption(c => c.strict = true),
      "use strict subtyping relations.",
    ),
    (
      "ignore",
      StrOption((c, s) => c.ignore = Some(s)),
      "ignore type mismatches in algorithms listed in a given JSON file.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var strict: Boolean = false,
    var ignore: Option[String] = None,
    var log: Boolean = false,
  )
}
