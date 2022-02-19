package esmeta.phase

import esmeta.*
import esmeta.spec.Spec
import esmeta.spec.util.{Parser => SpecParser}
import esmeta.js.*
import esmeta.js.util.{Parser => JSParser}
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*

/** `parse` phase */
case object JSParse extends Phase[Unit, Ast] {
  val name = "js-parse"
  val help = "parses a JavaScript file."
  def apply(
    unit: Unit,
    globalConfig: GlobalConfig,
    config: Config,
  ): Ast = {
    val (_, spec) =
      time("extract specification model", SpecParser.parseSpecWithVersion())
    val filename = getFirstFilename(globalConfig, "extract")
    val content = readFile(filename)
    val parser = JSParser(spec.grammar).parser("Script")
    parser(content)
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
