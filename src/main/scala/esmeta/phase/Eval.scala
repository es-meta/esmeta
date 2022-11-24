package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*

/** `eval` phase */
case object Eval extends Phase[CFG, State] {
  val name = "eval"
  val help = "evaluates an ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): State =
    if (config.multiple)
      var st = State(cfg, Context(cfg.main))
      for {
        path <- cmdConfig.targets
        file <- walkTree(path)
        filename = file.toString
        if jsFilter(filename)
      } st = run(cfg, config, filename)
      st
    else run(cfg, config, getFirstFilename(cmdConfig, this.name))

  def run(cfg: CFG, config: Config, filename: String): State = Interpreter(
    Initialize.fromFile(cfg, filename),
    log = config.log,
    timeLimit = config.timeLimit,
    tycheck = config.tycheck,
  )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "tycheck",
      BoolOption(c => c.tycheck = true),
      "turn on type check mode.",
    ),
    (
      "multiple",
      BoolOption(c => c.multiple = true),
      "execute multiple programs (result is the state of the last program).",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var timeLimit: Option[Int] = None,
    var tycheck: Boolean = false,
    var multiple: Boolean = false,
    var log: Boolean = false,
  )
}
