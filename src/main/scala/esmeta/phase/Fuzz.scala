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
import scala.collection.mutable.ArrayBuffer

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

    val parseFails: ArrayBuffer[String] = ArrayBuffer()
    val validationFails: ArrayBuffer[String] = ArrayBuffer()

    val validAsts = for {
      (raw, k) <- simpleSyn.initPool.zipWithIndex
      opt = optional(cfg.scriptParser.from(raw))
      filtered <- opt
      isValid = ValidityChecker(cfg.grammar, filtered)
      valid <- if (isValid) Some(filtered) else None
      _ = if (config.log && opt.isEmpty) parseFails += raw
      _ = if (config.log && !isValid) validationFails += raw
    } yield valid

    println(s"--- Filtered into ${validAsts.length} valid seeds")

    if (config.log) {
      println(s"--- Invalid seeds are logged into $FUZZ_DIR ...")
      log("parse-failures", parseFails)
      log("validation-failures", validationFails)
    }

    /** TODO fix the design of syntax coverage */
    /** Measure Syntax Coverage of seeds of simpleSyn */
    def auxAst(ast: Ast): Set[RhsNode] = ast match {
      case lex: Lexical => Set()
      case syn: Syntactic =>
        val childrenResults = syn.children.flatMap {
          case Some(childAst) => auxAst(childAst)
          case None           => Set()
        }.toSet
        childrenResults + getRhs(syn.name, syn.args, syn.rhsIdx)
    }
    val covered = validAsts
      .flatMap(ast =>
        ast match {
          case syn: Syntactic => syn.chains.map(auxAst).toSet
          case _              => Set()
        },
      )
      .reduce(_ ++ _)

    println(f"--- SyntaxCoverage: ${ratioString(covered.size, rhsNodes.size)}")

    val builtInSyn = BuiltinSynthesizer(cfg.spec.algorithms)
    println(s"=== BuiltInSyn: ${builtInSyn.initPool.length} seeds synthesized")

    val validSeeds = validAsts.map(_.toString(grammar = Some(grammar)).trim)
    val initPool = validSeeds ++ builtInSyn.initPool
    println(s"[*] Total ${initPool.length} seeds are synthesized")

    // TODO refactor and add features to Coverage
    println(s"--- Testing SemanticCoverage...")
    val target = "const array1 = [1, 2, 3];\narray1.shift();" // TEST
    lazy val cov = Coverage(cfg = cfg, timeLimit = Some(1))
    try cov.runWithSrc(target)
    catch { case e: Throwable => println("NotSupported feature detected") }
    println(cov.toString)
    ???

  /** logging mode */
  private def log(filename: String, fails: ArrayBuffer[String]): Unit = {
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
