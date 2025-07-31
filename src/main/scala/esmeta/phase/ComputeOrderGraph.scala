package esmeta.phase

import esmeta.*
import esmeta.analyzer.eoggen.*
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.spec.Spec
import esmeta.parser.ESParser
import esmeta.transpile.Transpiler
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `compute-eog` phase */
case object ComputeEOG extends Phase[CFG, Unit] {
  val name = "compute-eog"
  val help = "parses an ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    given CFG = cfg
    val filename = getFirstFilename(cmdConfig, name)
    val ast = ESParser(cfg.grammar, config.debug)("Script").fromFile(filename)
    val eogGenerator = new EOGGenerator(cfg, ast)
    eogGenerator.analyze
    eogGenerator.npMap
    ()

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
