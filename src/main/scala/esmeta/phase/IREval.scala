package esmeta.phase

import esmeta.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.*
import esmeta.ir.Program
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*

// TODO sort imports
import esmeta.peval.PartialEvaluator
import esmeta.peval.pstate.{PContext, PState}

/** `ir-eval` phase */
case object IREval extends Phase[Program, State] {
  val name = "ir-eval"
  val help = "evaluate an IR-ES (ESMeta Intermediate Representation) file."

  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): State = run(config, program)

  def run(config: Config, prog: Program): State =
    if (config.format) then
      val pw = getPrintWriter(s"$IREVAL_LOG_DIR/formatted.ir")
      pw.println(prog.toString())
      pw.close
    Interpreter(
      State(CFGBuilder(prog)),
      log = config.log,
      detail = false,
      logPW = Some(getPrintWriter(s"$IREVAL_LOG_DIR/log")),
      timeLimit = config.timeLimit,
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "format",
      BoolOption(_.format = _),
      "print the formatted input ir.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var format: Boolean = false,
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
  )
}
