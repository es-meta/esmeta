package esmeta.phase

import esmeta.*
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `parse` phase */
case object Parse extends Phase[Spec, Ast] {
  val name = "parse"
  val help = "parses an ECMAScript file."
  def apply(
    spec: Spec,
    cmdConfig: CommandConfig,
    config: Config,
  ): Ast =
    val filename = getFirstFilename(cmdConfig, name)
    ESParser(spec.grammar, config.debug)(config.goal).fromFile(filename)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(_.debug = _),
      "turn on debugging mode.",
    ),
    (
      "goal",
      StrOption(_.goal = _),
      "parsing goal: Script or Module (default: Script).",
    ),
  )
  case class Config(
    var debug: Boolean = false,
    var goal: String = "Script",
  )
}
