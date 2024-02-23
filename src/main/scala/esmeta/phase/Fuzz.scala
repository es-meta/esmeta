package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.parser.AstFrom
import esmeta.es.Ast
import esmeta.es.util.UnitWalker
import esmeta.es.util.Coverage
import esmeta.synthesizer.SimpleSynthesizer
import esmeta.synthesizer.BuiltinSynthesizer
import esmeta.spec.util.GrammarGraph
import esmeta.util.SystemUtils.*
import esmeta.spec.Grammar
import esmeta.es.Syntactic
import esmeta.es.Lexical

/** `fuzz` phase */
case object Fuzz extends Phase[CFG, Coverage] {
  val name = "fuzz"
  val help = "generate ECMAScript programs for fuzzing."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage =
    val graph = GrammarGraph(cfg.grammar)
    import graph.*

    val simpleSyn = SimpleSynthesizer(cfg.grammar)
    println(s"=== SimpleSyn: ${simpleSyn.initPool.length} seeds synthesized")

    // TODO Filter early-error seeds for avoiding error

    /** Measure Syntax Coverage of synthesized seeds of simpleSyn */
    val asts = simpleSyn.initPool.map(cfg.scriptParser.from(_))
    val covered = asts
      .map(
        _.chains
          .map(_ match
            case lex: Lexical =>
            case syn: Syntactic =>
              getRhs(syn.name, syn.args, syn.rhsIdx),
          )
          .toSet,
      )
      .reduce(_ ++ _)

    val percent = (covered.size.toDouble / rhsNodes.size) * 100
    println(s"=== Covered ${covered.size} rhsNodes (Total: ${rhsNodes.size})")
    println(f"=== SyntaxCoverage: ${percent}%.2f%%")

    if (percent == 100) println("--- [PASS] Covered all rhsNodes")
    else println("--- [FAIL] Uncovered rhsNodes remaining...")

    val builtInSyn = BuiltinSynthesizer(cfg.spec.algorithms)
    println(s"=== BuiltInSyn: ${builtInSyn.initPool.length} seeds synthesized")

    val initPool = simpleSyn.initPool ++ builtInSyn.initPool
    println(s"--- [PASS] ${initPool.length} seed programs are synthesized")
    ???

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
