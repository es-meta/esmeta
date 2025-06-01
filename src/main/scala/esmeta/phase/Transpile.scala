package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.spec.Spec
import esmeta.parser.ESParser
import esmeta.transpile.Transpiler
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `transpile` phase */
case object Transpile extends Phase[CFG, (Ast, CFG)] {
  val name = "transpile"
  val help = "parses an ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): (Ast, CFG) =
    val filename = getFirstFilename(cmdConfig, name)
    val ast = ESParser(cfg.grammar, config.debug)("Script").fromFile(filename)
    Transpiler(ast)(using cfg) -> cfg

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(_.debug = _),
      "turn on debugging mode.",
    ),
  )
  case class Config(
    var debug: Boolean = false,
  )
}
