package esmeta.phase

import esmeta.*
import esmeta.spec.*
import esmeta.spec.Utils.*
import esmeta.util.*
import esmeta.util.HtmlUtils.*
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
    val spec = config.json match {
      case Some(filename) => ???
      case None =>
        val filename = getFirstFilename(globalConfig, "extract")
        val content = readFile(filename)
        Parser.parseSpec(content)
    }

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
      if (config.stat) spec.stats.dumpTo(s"$EXTRACT_LOG_DIR/stat")
    }
    spec
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "json",
      StrOption((c, s) => c.json = Some(s)),
      "load specification from JSON.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "stat",
      BoolOption(c => {
        c.log = true
        c.stat = true
      }),
      "turn on stat mode.",
    ),
  )
  case class Config(
    var json: Option[String] = None,
    var log: Boolean = false,
    var stat: Boolean = false,
  )
}
