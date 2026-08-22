package esmeta.phase

import esmeta.*
import esmeta.analyzer.tychecker.TyChecker
import esmeta.cfg.*
import esmeta.es.util.Coverage
import esmeta.es.util.Coverage.Cond
import esmeta.ir.{Func => _, *}
import esmeta.solver.*
import esmeta.spec.{BuiltinHead, ParamKind}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Set => MSet, Queue}

/** `solve` phase */
case object Solve extends Phase[CFG, String] {
  val name = "solve"
  val help = "generates an ECMAScript program that covers a target branch"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): String =
    val id = config.branch.getOrElse(raise("solve: branch id is required"))
    val branch = cfg.nodeMap.get(id) match
      case Some(b: Branch) => b
      case _               => raise(s"solve: node $id is not a branch")
    val conds: List[Cond] = config.side match
      case Some(side) => List(Cond(branch, side))
      case None       => List(Cond(branch, true), Cond(branch, false))

    given CFG = cfg

    val entries = SymInterp.sortedEntries(branch)

    given SymInterpRunner =
      SymInterp(cfg, timeLimit = Some(10), detail = config.detail)
    given Coverage = Coverage(cfg, timeLimit = Some(10))

    conds
      .map { cond =>
        LazyList
          .from(entries)
          .flatMap(solve(_, branch, cond))
          .headOption match
          case Some(js) => s"[solve] $cond: $js"
          case None     => s"[solve] $cond: no solution"
      }
      .mkString("\n")

  /** symbolic execution -> synthesis -> validation */
  def solve(
    func: Func,
    branch: Branch,
    cond: Cond,
  )(using runner: SymInterpRunner, cov: Coverage): Option[String] =
    println(s"=== Entry: ${func.name} ===")
    val result =
      try {
        val interp = runner(func, cond)
        LazyList
          .continually(interp.nextCandidate)
          .takeWhile(_.isDefined)
          .flatMap(_ => interp.reifyAll.take(maxCandsPerPath).toList)
          .find(js => covers(js, cond))
      } catch {
        case e: Throwable => println(s"[error] ${func.name}: $e"); None
      }
    result match
      case Some(js) => println(s"[Solution] $js")
      case None     => println(s"[No solution]")
    result

  /** run and check it covers the target */
  def covers(js: String, cond: Cond)(using cov: Coverage): Boolean =
    try {
      val interp = cov.run(js)
      interp.touchedCondViews.keys.exists { cv =>
        cv.cond.branch.id == cond.branch.id && cv.cond.cond == cond.cond
      }
    } catch { case _: Throwable => false }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "branch",
      NumOption((c, k) => c.branch = Some(k)),
      "solve for the given branch id.",
    ),
    (
      "side",
      BoolOption((c, b) => c.side = Some(b)),
      "solve only the given side (default: both).",
    ),
    (
      "detail",
      BoolOption((c, b) => c.detail = b),
      "print detailed symbolic execution steps (default: false).",
    ),
  )
  case class Config(
    var branch: Option[Int] = None,
    var side: Option[Boolean] = None,
    var detail: Boolean = false,
  )

  private val maxCandsPerPath = 64
}
