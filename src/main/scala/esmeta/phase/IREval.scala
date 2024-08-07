package esmeta.phase

import esmeta.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.*
import esmeta.ir.Program
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*

/** `ir-eval` phase */
case object IREval extends Phase[Unit, State] {
  val name = "ir-eval"
  val help = "evaluate an IR-ES (ESMeta Intermediate Representation) file."

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): State = run(config, getFirstFilename(cmdConfig, this.name))

  def run(config: Config, filename: String): State =
    val prog = Program.fromFile(filename)
    if (config.format) then
      dumpFile(
        name = "the formatted IR-ES program",
        data = prog,
        filename = filename,
      )
    Interpreter(
      State(CFGBuilder(prog)),
      log = config.log,
      detail = false,
      logPW = Some(getPrintWriter(s"$IRINTERP_LOG_DIR/log")),
      timeLimit = config.timeLimit,
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "format",
      BoolOption(c => c.format = true),
      "format (reprint) the input ir file.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var format: Boolean = false,
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
  )
}
