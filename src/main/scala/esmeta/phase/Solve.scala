package esmeta.phase

import esmeta.*
import esmeta.cfg.{Branch, CFG}
import esmeta.es.util.JsonProtocol
import esmeta.es.util.Coverage.{Cond, CondView, CondViewInfo}
import esmeta.solver.Solver
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `solve` phase */
case object Solve extends Phase[CFG, Unit] {
  val name = "solve"
  val help = "generates ECMAScript programs for selected builtin branch sides"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    config.seed.foreach(setSeed)
    val solver = new Solver(
      cfg,
      branch = config.branch,
      side = config.side,
      log = config.log,
      detail = config.detail,
    )
    val witnesses = solver.solve
    val programs = witnesses.toList.sortBy(_._1)
    for (dir <- solver.logDir)
      dumpPrograms(
        cfg,
        dir,
        programs.map {
          case ((id, side), js) =>
            cfg.nodeMap.get(id) match
              case Some(branch: Branch) => Cond(branch, side) -> js
              case _ => raise(s"solve: node $id is not a branch")
        },
      )
    solver.report(witnesses)

  /** dump numbered programs with a branch coverage file */
  def dumpPrograms(
    cfg: CFG,
    dir: String,
    entries: List[(Cond, String)],
  ): Unit = {
    val programs = entries
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
    val coverage = entries.zipWithIndex.map {
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
      "seed",
      NumOption((c, k) => c.seed = Some(k)),
      "set the specific seed for the random number generator (default: None).",
    ),
  )
  case class Config(
    var branch: Option[Int] = None,
    var side: Option[Boolean] = None,
    var log: Boolean = false,
    var detail: Boolean = false,
    var seed: Option[Int] = None,
  )
}
