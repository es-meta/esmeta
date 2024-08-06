package esmeta.phase

import esmeta.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.*
import esmeta.ir.Program
import esmeta.peval.PartialEvaluator
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*

/** `irpeval` phase */
case object IRPeval extends Phase[Unit, Unit] {
  val name = "ir-peval"
  val help =
    "partial-evaluate an IR-ES (ESMeta IR) program, and print it to log"

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = run(config, getFirstFilename(cmdConfig, this.name))

  def run(config: Config, filename: String): Unit =
    val pevaled =
      PartialEvaluator(Program.fromFile(filename), log = true).result
    for (filename <- config.out)
      dumpFile(
        name = "the partialy evaluated IR-ES program",
        data = pevaled,
        filename = filename,
      )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, k) => c.out = Some(k)),
      "set the filepath to print partial-evalated program (default: no print).",
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
    var out: Option[String] = None,
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
  )
}
