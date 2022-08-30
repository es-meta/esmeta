package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interpreter.*
import esmeta.es.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.es.util.mutator.*

/** `mutate` phase */
case object Mutate extends Phase[CFG, String] {
  val name = "mutate"
  val help = "mutates a ECMAScript program."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String =
    val filename = getFirstFilename(cmdConfig, this.name)
    val ast = ESParser(cfg.spec.grammar)("Script").fromFile(filename)
    val mutator: Mutator = RandomMutation(ast)
    val synth: RandomSynth = RandomSynth(cfg.spec.grammar)
    synth.synthesize("AssignmentExpression", "RelationalExpression")
    // mutator.mutate.toString
    mutator.mutate.toString(grammar = Some(cfg.grammar))

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
