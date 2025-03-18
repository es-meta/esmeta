package esmeta.phase

import esmeta.*
import esmeta.analyzer.tychecker.*
import esmeta.cfg.{CFG, Func}
import esmeta.error.TyCheckFail
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `tycheck` phase */
case object TyCheck extends Phase[CFG, Unit] {
  val name = "tycheck"
  val help = "performs a type checking of ECMA-262."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    import TyChecker.*
    val silent = cmdConfig.silent
    val tychecker = TyChecker(
      cfg = cfg,
      targetPattern = config.target,
      inferTypeGuard = config.inferTypeGuard,
      useProvenance = config.useProvenance,
      useSyntacticKill = config.useSyntacticKill,
      typeSens = config.typeSens,
      config = TyChecker.Config(),
      ignore = config.ignorePath.fold(Ignore())(Ignore.apply),
      log = config.log,
      detail = config.detail,
      silent = silent,
      useRepl = config.useRepl,
      replContinue = config.replContinue,
    )
    tychecker.analyze
    if (tychecker.needUpdate)
      if (config.updateIgnore) tychecker.updateIgnore
      throw TyCheckFail(if (silent) None else Some(tychecker.getMessage))

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target of type checking with a regular expression pattern.",
    ),
    (
      "repl",
      BoolOption(_.useRepl = _),
      "use a REPL for type checking of ECMA-262.",
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
      "type sensitivity for arguments.",
    ),
    (
      "infer-guard",
      BoolOption(_.inferTypeGuard = _),
      "automatic inference of type guards (default: true).",
    ),
    (
      "provenance",
      BoolOption(_.useProvenance = _),
      "turn on provenance tracking.",
    ),
    (
      "syntactic-kill",
      BoolOption(_.useSyntacticKill = _),
      "use syntactic kill for type checking(as Kent's work).",
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
    var inferTypeGuard: Boolean = true,
    var useProvenance: Boolean = false,
    var useSyntacticKill: Boolean = false,
  )
}
