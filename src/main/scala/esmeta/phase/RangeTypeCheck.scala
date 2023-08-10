package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.spec.Spec.*
import scala.io.StdIn
import esmeta.error.TypeCheckFail
import esmeta.error.ESMetaError
import esmeta.error.NoTargetError

case object RangeTypeCheck extends Phase[Unit, Unit] {
  val name = "range-tycheck"

  val help =
    "performs a type analysis of ECMA-262 through multiple versions of specification."

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val targets = getAllTargets(cmdConfig.targets)
    if (!config.yes)
      val confirm = StdIn.readLine(
        s"Are you sure to run analysis for ${targets.length} revisions? (y/n): ",
      )
      if (confirm.take(1) != "y")
        return println("Canceled.")

    val targetLen = targets.length
    for ((v @ Version(name, hash), idx) <- targets.zipWithIndex) {
      val logdir =
        s"$LOG_DIR/range-tycheck/${s"%0${targetLen.toString.length}d"
          .format(idx + 1)}-${v.shortHash}"
      print(s"[${idx + 1}/${targetLen}]: $name (hash: $hash)")
      try {
        val result = executeCmd(
          s"""esmeta tycheck -silent -extract:target="$hash" -tycheck:log -tycheck:logdir="${logdir}" -tycheck:level=${config.level}""",
        )
      } catch {
        case e => print(" - failed " + e)
      }
      println
    }

    if (!cmdConfig.silent) println("Done.")

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "yes",
      BoolOption((c) => c.yes = true),
      "answer 'yes' to any prompts automatically.",
    ),
    (
      "level",
      NumOption((c, k) => c.level = k),
      "turn on alarms for type errors whose alarm level is " +
      "lower than or equal to the given number (default: 1)",
    ),
  )
  case class Config(
    var yes: Boolean = false,
    var level: Int = 1,
  )
}