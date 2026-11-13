package esmeta.phase

import esmeta.*
import esmeta.opcount.CountYets as Counter
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `count-yets` phase */
case object CountYets extends Phase[Unit, List[Counter.Result]] {
  val name = "count-yets"
  val help =
    "counts unparsed (yet) steps in all algorithms of ECMA-262 revisions."
  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): List[Counter.Result] =
    val fromFile = config.targetFile.toList.flatMap { file =>
      readFile(file).linesIterator.map(_.trim).filter(_.nonEmpty).toList
    }
    val targets = cmdConfig.targets ++ fromFile
    if (targets.isEmpty) raise("`count-yets` phase needs at least one target")
    Counter.run(targets, config.repo, config.out)

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump summary.json, summary.txt, and details.json into the directory " +
      s"(default: $COUNT_YETS_LOG_DIR).",
    ),
    (
      "targets",
      StrOption((c, s) => c.targetFile = Some(s)),
      "read additional targets from the file (one per line).",
    ),
    (
      "repo",
      StrOption((c, s) => c.repo = s),
      s"ECMA-262 git repository (default: $ECMA262_DIR).",
    ),
  )
  case class Config(
    var out: Option[String] = Some(COUNT_YETS_LOG_DIR),
    var targetFile: Option[String] = None,
    var repo: String = ECMA262_DIR,
  )
}
