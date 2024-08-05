package esmeta.phase

import esmeta.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.*
import esmeta.ir.Program
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*

/** `irpeval` phase */
case object IRPeval extends Phase[Unit, Unit] {
  val name = "irpeval"
  val help =
    "partial-evaluate an IR-ES (ESMeta IR) program, and print it to log"

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = ()
  // run(config, getFirstFilename(cmdConfig, this.name))

  // def run(config: Config, filename: String): State =
  //   Interpreter(
  //     State(CFGBuilder(Program.fromFile(filename))),
  //     log = false,
  //     detail = false,
  //     timeLimit = config.timeLimit,
  //   )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
  )
  case class Config(
    var timeLimit: Option[Int] = None,
  )
}
