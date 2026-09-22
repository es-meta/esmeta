package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.solver.Solver
import esmeta.util.*

/** `solve` phase */
case object Solve extends Phase[CFG, String] {
  val name = "solve"
  val help = "generates ECMAScript programs for selected builtin branch sides"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): String =
    Ablation.noShape = config.noShape
    Ablation.noTemplate = config.noTemplate
    new Solver(
      cfg,
      branch = config.branch,
      side = config.side,
      log = config.log,
      detail = config.detail,
    ).result

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
    var noShape: Boolean = false,
    var noTemplate: Boolean = false,
  )
}
