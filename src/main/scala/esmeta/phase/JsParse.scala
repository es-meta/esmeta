package esmeta.phase

import esmeta.*
import esmeta.spec.Spec
import esmeta.js.*
import esmeta.js.util.*

/** `parse` phase */
case object JsParse extends Phase[Unit, Ast] {
  val name = "js-parse"
  val help = "parses a JavaScript file."
  def apply(
    unit: Unit,
    globalConfig: GlobalConfig,
    config: Config,
  ): Ast = {
    val spec: Spec = ???
    val ast: Ast = ???
    ast
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
