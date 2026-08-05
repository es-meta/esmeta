package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.fuzzer.mutator.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import java.util.concurrent.TimeoutException
import scala.Console.*

/** `mutate` phase */
case object Mutate extends Phase[CFG, String] {
  val name = "mutate"
  val help = "mutates an ECMAScript program."
  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): String =
    import Coverage.*

    val filename = getFirstFilename(cmdConfig, this.name)
    val code = readFile(filename)

    val cov = Coverage(cfg, kFs = config.kFs, timeLimit = Some(1))

    // detect which side of the target branch is covered by the given program
    val coveredCondView: Option[CondView] = config.targetBranchId.map { id =>
      val interp = cov.run(code)
      // update targetCondViews on the coverage object
      cov.check(Script(Code.Simple(code), "seed", true), interp)
      interp.touchedCondViews.keySet
        .filter(_.cond.branch.id == id)
        .headOption
        .getOrElse(raise(s"branch $id is not covered by the given program"))
    }

    val targetCondView = coveredCondView.map(_.neg)

    // print localization info
    for (cv <- coveredCondView) {
      val funcName = cfg.funcOf.get(cv.cond.branch).map(_.name).getOrElse("?")
      val targets = cov.targetCondViews
        .getOrElse(cv.cond, Map())
        .getOrElse(cv.view, Set())
      println(s"[mutate] covered: ${cv.cond.simpleString} (@ $funcName)")
      println(s"[mutate] target: ${cv.neg.cond.simpleString} (@ $funcName)")
      println(s"[mutate] program: ${setColor(CYAN)(code.trim())}")
      if (targets.isEmpty) println(s"[mutate] no targets found for this branch")
      else
        targets.foreach { t =>
          val snippet = setColor(CYAN)(t.loc.getString(code))
          val astInfo = setColor(YELLOW)(s"$t ${t.loc}")
          println(s"[mutate] localized to $astInfo: $snippet")
        }
    }

    given CFG = cfg
    val mutator: Mutator = RandomMutator()
    var blocked = Set[String]()
    var iter = 0

    val startTime = System.currentTimeMillis
    def elapsed = System.currentTimeMillis - startTime
    def timeout = config.duration.fold(false)(_ * 1000 < elapsed)
    def trialExceeded = config.trial.fold(false)(iter >= _)
    def checkLimit(): Unit =
      if (timeout) throw TimeoutException("mutate: timeout")
      if (trialExceeded) throw TimeoutException("mutate: trial exceeded")

    def nextMutant(): String =
      val result = mutator(code, coveredCondView.map((_, cov))).code
      val ppResult = s"----- iter: $iter -----> ${result.trim()}"
      iter += 1
      if (config.debug) println(ppResult)
      result

    def coversTarget(code: String): Boolean = targetCondView.exists { cv =>
      try {
        val covered = cov.run(code).touchedCondViews.keySet.contains(cv)
        if (covered) println(s"[mutate] Covered in $iter iterations")
        covered
      } catch { case _: Exception => false }
    }

    var mutatedCode = nextMutant()

    // repeat until the mutated program is valid and covers target
    if (coveredCondView.isDefined)
      try {
        while (!(ValidityChecker(mutatedCode) && coversTarget(mutatedCode))) {
          checkLimit()
          mutatedCode = nextMutant()
          while (blocked.contains(mutatedCode))
            checkLimit()
            mutatedCode = nextMutant()
          blocked += mutatedCode
        }
      } catch {
        case e: TimeoutException =>
          println(s"[mutate] ${e.getMessage} after $iter iterations")
          mutatedCode = setColor(RED)("FAILED TO MUTATE WITHIN LIMIT")
      }

    // dump the mutated ECMAScript program
    for (filename <- config.out)
      dumpFile(
        name = "the mutated ECMAScript program",
        data = mutatedCode,
        filename = filename,
      )

    mutatedCode

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the mutated ECMAScript program to a given path.",
    ),
    (
      "branch",
      NumOption((c, k) => c.targetBranchId = Some(k)),
      "target branch id to cover the uncovered side.",
    ),
    (
      "kfs",
      NumOption((c, k) => c.kFs = k),
      "feature sensitivity level for targeting (default: 0).",
    ),
    (
      "duration",
      NumOption((c, k) => c.duration = Some(k)),
      "set the maximum duration for mutation in seconds (default: 300).",
    ),
    (
      "trial",
      NumOption((c, k) => c.trial = Some(k)),
      "set the maximum number of mutation trials (default: INF).",
    ),
    (
      "debug",
      BoolOption((c, b) => c.debug = b),
      "print each generated mutant (default: false).",
    ),
  )
  class Config(
    var out: Option[String] = None,
    var targetBranchId: Option[Int] = None,
    var kFs: Int = 0,
    var duration: Option[Int] = None,
    var trial: Option[Int] = None,
    var debug: Boolean = false,
  )
}
