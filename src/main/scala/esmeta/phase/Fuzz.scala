package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.es.util.{UnitWalker, Coverage, ValidityChecker}
import esmeta.spec.util.GrammarGraph
import esmeta.synthesizer.{SimpleSynthesizer, BuiltinSynthesizer}
import scala.collection.mutable.ListBuffer

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

    var parseFails = new ListBuffer[String]()
    var validationFails = new ListBuffer[String]()

    // TODO refactoring
    val validSeeds = for {
      (raw, k) <- simpleSyn.initPool.zipWithIndex
      opt = optional(cfg.scriptParser.from(raw))
      _ = if (opt.isEmpty) parseFails += raw
      filtered <- opt
      isValid = ValidityChecker(cfg.grammar, filtered)
      _ = if (!isValid) validationFails += raw
      valid <- if (isValid) Some(filtered) else None
    } yield valid

    println(s"--- Filtered into ${validSeeds.length} valid seeds")

    if (config.log) {
      println(s"--- Invalid seeds are logged into $FUZZ_DIR ...")
      log("parse-failures", parseFails)
      log("validation-failures", validationFails)
    }

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

  /** logging mode */
  private def log(filename: String, fails: ListBuffer[String]): Unit = {
    mkdir(FUZZ_DIR)
    dumpFile(
      data = fails.sorted.mkString(LINE_SEP),
      filename = s"$FUZZ_DIR/$filename",
    )
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var log: Boolean = false,
  )
}
