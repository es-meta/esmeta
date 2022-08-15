package esmeta.phase

import esmeta.*
import esmeta.spec.*
import esmeta.spec.util.*
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `extract` phase */
case object Extract extends Phase[Unit, Spec] {
  val name = "extract"
  val help = "extracts specification model from ECMA-262 (spec.html)."
  def apply(
    unit: Unit,
    globalConfig: GlobalConfig,
    config: Config,
  ): Spec = {
    val spec = Parser.parseSpecWithVersion(config.version)

    // logging mode
    if (config.log) {
      mkdir(EXTRACT_LOG_DIR)

      val yets = spec.incompleteSteps
      dumpFile(
        name = "not yet supported steps",
        data = yets
          .map(_.toString(detail = false, location = false))
          .sorted
          .mkString("\n"),
        filename = s"$EXTRACT_LOG_DIR/yets",
      )

      dumpFile(
        name = "the summary of extracted specification",
        data = spec,
        filename = s"$EXTRACT_LOG_DIR/summary",
      )

      // dump statistics
      spec.stats.dumpTo(s"$EXTRACT_LOG_DIR/stat")
    }
    spec
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "version",
      StrOption((c, s) => c.version = Some(s)),
      "parse specification with versions.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode",
    ),
  )
  case class Config(
    var version: Option[String] = None,
    var log: Boolean = false,
  )
}
