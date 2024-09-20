package esmeta.phase

import esmeta.*
import esmeta.analyzer.*
import esmeta.analyzer.TypeAnalyzer.Ignore
import esmeta.analyzer.domain
import esmeta.cfg.{CFG, Func}
import esmeta.error.TypeCheckFail
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `tycheck` phase */
case object TypeCheck extends Phase[CFG, TypeAnalyzer#Semantics] {
  val name = "tycheck"
  val help = "performs a type analysis of ECMA-262."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): TypeAnalyzer#Semantics =
    val silent = cmdConfig.silent
    val analyzer: TypeAnalyzer = TypeAnalyzer(
      cfg = cfg,
      targetPattern = config.target,
      typeSens = config.typeSens,
      useTypeGuard = config.typeGuard,
      config = TypeAnalyzer.Config(),
      ignore = config.ignorePath.fold(Ignore())(Ignore.apply),
      log = config.log,
      detail = config.detail,
      silent = silent,
      useRepl = config.useRepl,
      replContinue = config.replContinue,
    )
    analyzer.analyze
    if (analyzer.needUpdate)
      if (config.updateIgnore) analyzer.updateIgnore
      throw TypeCheckFail(if (silent) None else Some(analyzer.toString))
    analyzer.sem

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target of type analysis with a regular expression pattern.",
    ),
    (
      "repl",
      BoolOption(_.useRepl = _),
      "use a REPL for type analysis of ECMA-262.",
    ),
    (
      "repl-continue",
      BoolOption(_.replContinue = _),
      "run `continue` command at startup when using REPL",
    ),
    (
      "ignore",
      StrOption((c, s) => c.ignorePath = Some(s)),
      "ignore type errors in algorithms listed in a given JSON file.",
    ),
    (
      "update-ignore",
      BoolOption(_.updateIgnore = _),
      "update the given JSON file used in ignoring type errors.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "logging mode with detailed information.",
    ),
    (
      "type-sens",
      BoolOption(_.typeSens = _),
      "type sensitivity (not-yet supported).",
    ),
    (
      "type-guard",
      BoolOption(_.typeGuard = _),
      "automatic inference of type guards (default: true).",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var ignorePath: Option[String] = None,
    var updateIgnore: Boolean = false,
    var useRepl: Boolean = false,
    var replContinue: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var typeSens: Boolean = false,
    var typeGuard: Boolean = true,
  )
}
