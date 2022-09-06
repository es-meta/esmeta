package esmeta.phase

import esmeta.*
import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `type-check` phase */
case object TypeCheck extends Phase[CFG, AbsSemantics] {
  val name = "type-check"
  val help = "performs a type analysis of ECMA-262."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): AbsSemantics =
    val allFuncs = cfg.funcs.filter(_.isParamTysDefined)
    val funcs = config.target.fold(allFuncs)(pattern => {
      val funcs = allFuncs.filter(f => pattern.r.matches(f.name))
      if (funcs.isEmpty)
        warn(s"failed to find functions matched with the pattern `$pattern`.")
      funcs
    })
    println(s"- ${funcs.size} target functions.")
    val sem = TypeAnalyzer(cfg, funcs)
    // print types of functions
    sem
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
  )
  case class Config(
    var target: Option[String] = None,
  )
}
