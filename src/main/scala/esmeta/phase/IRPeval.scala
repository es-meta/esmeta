package esmeta.phase

import esmeta.{CommandConfig, IRPEVAL_LOG_DIR}
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.*
import esmeta.ir.Program
import esmeta.peval.PartialEvaluator
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
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
  ): Unit =
    if (config.outAuto && config.out.isDefined)
      error(
        "Turning on both out-auto option (-ir-peval:out-auto) and " +
        "the out option (-ir-peval:out) with is not allowed.",
      )

    run(config, getFirstFilename(cmdConfig, this.name))

  def run(config: Config, filename: String): Unit =
    val pevaled = PartialEvaluator(Program.fromFile(filename), log = true)
    for (
      filename <-
        (if config.outAuto then Some(s"$IRPEVAL_LOG_DIR/out.ir")
         else config.out)
    )
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
      "out-auto",
      BoolOption((c) => c.outAuto = true),
      "use 'logs/ir-peval/out.ir' as filepath to print partial-evaluated program.",
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
    var outAuto: Boolean = false,
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
  )
}
