package esmeta.phase

import esmeta.*
import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.cfg.{CFG, Func}
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
    val targets = getInitTargets(cfg, config.target)
    println(s"- ${targets.size} functions are initial targets.")
    val sem = TypeAnalyzer(cfg, targets)
    println(sem.shortString)
    if (config.log) log(sem)
    sem

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

  // logging mode
  private def log(sem: AbsSemantics): Unit = {
    mkdir(ANALYZE_LOG_DIR)
    dumpFile(
      name = "type analysis result",
      data = sem,
      filename = s"$ANALYZE_LOG_DIR/type-analysis",
    )
  }

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
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var log: Boolean = false,
  )
}
