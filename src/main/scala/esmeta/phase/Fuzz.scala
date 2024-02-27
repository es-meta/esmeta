package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.parser.AstFrom
import esmeta.es.Ast
import esmeta.es.util.UnitWalker
import esmeta.es.util.Coverage
import esmeta.synthesizer.SimpleSynthesizer
import esmeta.synthesizer.BuiltinSynthesizer
import esmeta.es.util.ValidityChecker
import esmeta.spec.util.GrammarGraph
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.spec.Grammar
import esmeta.es.Syntactic
import esmeta.es.Lexical
import io.circe.JsonObject

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

    mkdir(FUZZ_DIR)
    // TODO refactoring
    val validSeeds = for {
      (raw, k) <- simpleSyn.initPool.zipWithIndex
      opt = optional(cfg.scriptParser.from(raw))
      _ = if (opt.isEmpty) dumpFile(raw, s"$FUZZ_DIR/$k.js")
      filtered <- opt
      isValid = ValidityChecker(cfg.grammar, filtered)
      _ = if (!isValid) dumpFile(raw, s"$FUZZ_DIR/$k.js")
      valid <- if (isValid) Some(filtered) else None
    } yield valid

    println(s"--- Filtered into ${validSeeds.length} valid seeds")
    println(s"--- Invalid seeds are logged into $FUZZ_DIR ...")

    /** Measure Syntax Coverage of seeds of simpleSyn */
    def processAst(ast: Ast): Set[RhsNode] = ast match {
      case lex: Lexical => Set()
      case syn: Syntactic =>
        val childrenResults = syn.children.flatMap {
          case Some(childAst) => processAst(childAst)
          case None           => Set()
        }.toSet
        childrenResults + getRhs(syn.name, syn.args, syn.rhsIdx)
    }
    val covered = validSeeds
      .flatMap(ast =>
        ast match {
          case syn: Syntactic => syn.chains.map(processAst).toSet
          case _              => Set()
        },
      )
      .reduce(_ ++ _)

    println(f"--- SyntaxCoverage: ${ratioString(covered.size, rhsNodes.size)}")

    val builtInSyn = BuiltinSynthesizer(cfg.spec.algorithms)
    println(s"=== BuiltInSyn: ${builtInSyn.initPool.length} seeds synthesized")

    val initPool = validSeeds ++ builtInSyn.initPool
    println(s"[*] Total ${initPool.length} seeds are synthesized")
    ???

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
