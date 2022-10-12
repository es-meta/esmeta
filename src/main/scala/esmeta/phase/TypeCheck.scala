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
  ): AbsSemantics =
    val targets = getInitTargets(cfg, config.target)
    val ignoreSet = optional {
      readJson[Set[String]](config.ignore.get)
    }.getOrElse(Set())
    println(s"- ${targets.size} functions are initial targets.")
    TypeAnalyzer(cfg, targets, ignoreSet, config.log)

  // find initial analysis targets based on a given regex pattern
  private def getInitTargets(cfg: CFG, target: Option[String]): List[Func] =
    // find all possible initial analysis target functions
    val allFuncs = cfg.funcs.filter(_.isParamTysDefined)
    target.fold(allFuncs)(pattern => {
      val funcs = allFuncs.filter(f => pattern.r.matches(f.name))
      if (funcs.isEmpty)
        warn(s"failed to find functions matched with the pattern `$pattern`.")
      funcs
    })

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
    var ignore: Option[String] = None,
    var log: Boolean = false,
  )
}
