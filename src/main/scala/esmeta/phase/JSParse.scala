package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.js.*
import esmeta.js.util.{Parser => JSParser}
import esmeta.util.SystemUtils.*

/** `parse` phase */
case object JSParse extends Phase[CFG, Ast] {
  val name = "js-parse"
  val help = "parses a JavaScript file."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): Ast =
    val spec = cfg.program.spec
    val filename = getFirstFilename(globalConfig, name)
    JSParser(spec.grammar)("Script").fromFile(filename)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
