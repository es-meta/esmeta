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

/** `ir-peval` phase */
case object IRPEval extends Phase[Program, Program] {
  val name = "ir-peval"
  val help =
    "p-eval zero-arity funcs in Program. Skip by default if not turned on explicitly."

  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): Program = run(config, program)

  def run(config: Config, prog: Program): Program =
    if (!config.use) then prog
    else
      Program(
        prog.funcs.map {
          case f if !f.params.isEmpty => f
          case f => {
            val (renamer, pst) = PartialEvaluator.prepare(f)
            val pev = PartialEvaluator(
              program = prog,
              log = config.log,
              detail = false, // TODO add to Config
              simplifyLevel = config.simplify,
              logPW = Some(getPrintWriter(s"$IRPEVAL_LOG_DIR/log")),
              timeLimit = None,
              renamer = renamer,
            )
            val (newFunc, _) = pev.run(f, pst, None)
            if (config.log) then
              val pw = getPrintWriter(s"$IRPEVAL_LOG_DIR/${f.name}.ir")
              pw.println(newFunc.toString())
              pw.flush
              pw.close
            newFunc
          }
        },
        prog.spec,
      )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
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
    (
      "turn-on",
      BoolOption(_.use = _),
      "turn on peval mode. (if not given, ir-peval skipped by default.)",
    ),
    (
      "s",
      NumOption(_.simplify = _),
      """set level of simplify strategy.
    0: do nothing
    1 : flatten
    2 (default): flatten & syntactic optimization (reference transparency)
    3: flatten & syntactic & semantic optimization (use-def chain)""",
    ),
  )
  case class Config(
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
    var use: Boolean = false,
    var simplify: Int = 1,
  )
}
