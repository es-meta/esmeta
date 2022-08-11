package esmeta.phase

import esmeta.*
import esmeta.js.*
import esmeta.js.util.{Parser => JSParser}
import esmeta.spec.Spec
import esmeta.util.SystemUtils.*

/** `parse` phase */
case object Parse extends Phase[Spec, Ast] {
  val name = "parse"
  val help = "parses a JavaScript file."
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): Ast =
    val filename = getFirstFilename(globalConfig, name)
    JSParser(spec.grammar)("Script").fromFile(filename)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
