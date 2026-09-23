package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.JsonProtocol
import esmeta.es.util.Coverage.{Cond, CondView, CondViewInfo}
import esmeta.solver.{Reducer, Solver}
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `solve` phase */
case object Solve extends Phase[CFG, Unit] {
  val name = "solve"
  val help = "generates ECMAScript programs for selected builtin branch sides"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    Ablation.noShape = config.noShape
    Ablation.noTemplate = config.noTemplate
    val solver = new Solver(
      cfg,
      branch = config.branch,
      side = config.side,
      log = config.log,
      detail = config.detail,
    )
    val solved = solver.solve
    val witnesses = if (config.reduce) Reducer(cfg)(solved) else solved
    for (dir <- solver.logDir)
      dumpWitnesses(cfg, dir, solver.targeted(witnesses))
    solver.report(witnesses)

  /** dump witnesses as numbered programs with a branch coverage file */
  def dumpWitnesses(
    cfg: CFG,
    dir: String,
    witnesses: List[(Cond, String)],
  ): Unit = {
    val programs = witnesses
      .map(_._2)
      .distinct
      .zipWithIndex
      .map { (js, index) => js -> (index + 1) }
    val programIds = programs.toMap
    dumpDir[(String, Int)](
      name = s"${programs.size} ECMAScript programs",
      iterable = programs,
      dirname = s"$dir/programs",
      getName = { case (_, id) => s"$id.js" },
      getData = { case (js, _) => js },
    )
    // reuse the Fuzzer coverage format
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given
    val coverage = witnesses.zipWithIndex.map {
      case ((cond, js), index) =>
        CondViewInfo(
          index,
          CondView(cond, None),
          s"programs/${programIds(js)}.js",
        )
    }
    dumpJson(
      name = "branch coverage",
      data = coverage,
      filename = s"$dir/branch-coverage.json",
    )
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "branch",
      NumOption((c, k) => c.branch = Some(k)),
      "target the branch to solve (default: all).",
    ),
    (
      "side",
      BoolOption((c, b) => c.side = Some(b)),
      "target the side to solve (requires -solve:branch; default: both).",
    ),
    (
      "log",
      BoolOption((c, b) => c.log = b),
      "turn on logging mode (default: false).",
    ),
    (
      "detail-log",
      BoolOption((c, b) => c.detail = b),
      "logging mode with detailed information.",
    ),
    (
      "reduce",
      BoolOption((c, b) => c.reduce = b),
      "reduce the witnesses after solving (default: false).",
    ),
    (
      "no-shape",
      BoolOption((c, b) => c.noShape = b),
      "ablation: drop the object structure from the type domain.",
    ),
    (
      "no-template",
      BoolOption((c, b) => c.noTemplate = b),
      "ablation: drop the call templates derived from the specification.",
    ),
  )
  case class Config(
    var branch: Option[Int] = None,
    var side: Option[Boolean] = None,
    var log: Boolean = false,
    var detail: Boolean = false,
    var reduce: Boolean = false,
    var noShape: Boolean = false,
    var noTemplate: Boolean = false,
  )
}
