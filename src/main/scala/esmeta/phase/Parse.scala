package esmeta.phase

import esmeta.*
import esmeta.es.*
import esmeta.parser.{Parser => ESParser}
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `parse` phase */
case object Parse extends Phase[Spec, Ast] {
  val name = "parse"
  val help = "parses an ECMAScript file."
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): Ast =
    val filename = getFirstFilename(globalConfig, name)
    ESParser(spec.grammar, config.debug)("Script").fromFile(filename)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on debugging mode.",
    ),
  )
  case class Config(
    var debug: Boolean = false,
  )
}
