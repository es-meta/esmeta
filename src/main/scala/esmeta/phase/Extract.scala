package esmeta.phase

import esmeta.*
import esmeta.spec.*
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
  ): Spec = config.json match {
    case Some(filename) => ???
    case None =>
      val filename = getFirstFilename(globalConfig, "extract")
      val content = readFile(filename)
      Parser.parseSpec(content)
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "json",
      StrOption((c, s) => c.json = Some(s)),
      "load specification from JSON.",
    ),
  )
  case class Config(
    var json: Option[String] = None,
  )
}
