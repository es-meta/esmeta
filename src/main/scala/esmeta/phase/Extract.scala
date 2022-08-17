package esmeta.phase

import esmeta.*
import esmeta.extractor.Extractor
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `extract` phase */
case object Extract extends Phase[Unit, Spec] {
  val name = "extract"
  val help = "extracts specification model from ECMA-262 (spec.html)."
  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Spec = {
    val spec = Extractor(config.target)

    // logging mode
    if (config.log) {
      mkdir(EXTRACT_LOG_DIR)

      val yets = spec.incompleteSteps
      dumpFile(
        name = "not yet supported steps",
        data = yets
          .map(_.toString(detail = false, location = false))
          .sorted
          .mkString(LINE_SEP),
        filename = s"$EXTRACT_LOG_DIR/yets",
      )

      dumpFile(
        name = "the summary of extracted specification",
        data = spec,
        filename = s"$EXTRACT_LOG_DIR/summary",
      )

      spec.stats.dumpTo(s"$EXTRACT_LOG_DIR/stat")
    }
    spec
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target git version of ECMA-262.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var log: Boolean = false,
  )
}
