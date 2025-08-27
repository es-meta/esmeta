package esmeta.phase

import esmeta.*
import esmeta.cfg.{CFG, Func}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.analyzer.paramflow.ParamFlowAnalyzer

/** `param-flow` phase */
case object ParamFlow extends Phase[CFG, Unit] {
  val name = "param-flow"
  val help = "performs an analysis for flow of parameters."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    import ParamFlowAnalyzer.*
    val silent = cmdConfig.silent
    val analyzer = ParamFlowAnalyzer(
      cfg = cfg,
      targetPattern = config.target,
      log = config.log,
      detail = config.detail,
      useRepl = config.useRepl,
      silent = silent,
    )
    analyzer.analyze

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
      "log",
      BoolOption(_.log = _),
      "logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "logging mode with detailed information.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var useRepl: Boolean = false,
    var replContinue: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
  )
}
