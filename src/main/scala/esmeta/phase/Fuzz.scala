package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.Coverage
import esmeta.synthesizer.BuiltinSynthesizer
import esmeta.spec.util.GrammarGraph

/** `fuzz` phase */
case object Fuzz extends Phase[CFG, Coverage] {
  val name = "fuzz"
  val help = "generate ECMAScript programs for fuzzing."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage =
    // for (code <- BuiltinSynthesizer(cfg.spec.algorithms).initPool)
    //   println(code)
    println(GrammarGraph(cfg.grammar))
    ???

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
