package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.mutator.*
import esmeta.parser.ESParser
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `mutate` phase */
case object Mutate extends Phase[CFG, String] {
  val name = "mutate"
  val help = "mutates an ECMAScript program."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String =
    val grammar = cfg.grammar
    val filename = getFirstFilename(cmdConfig, this.name)
    val ast = cfg.scriptParser.fromFile(filename)
    val mutator = config.builder(grammar)

    // get a mutated AST
    var mutatedAst = mutator(ast)

    // repeat until the mutated program becomes valid when
    // `-mutate:untilValid` is turn on
    while (config.untilValid && !mutatedAst.valid(grammar))
      mutatedAst = mutator(ast)

    // `-mutate:dedup` is turn on
    if config.dedup then
      val patterns = List("ObjectBindingPattern:1") // hardcoded pattern
      mkdir(DEDUP_LOG_DIR)
      val threshold = 100; // !debug
      val pool = scala.collection.mutable.Set[String]()
      while (pool.size < threshold)
        mutatedAst = mutator(ast)
        while (!mutatedAst.contains(patterns))
          mutatedAst = mutator(ast)
        if (mutatedAst.valid(grammar))
          pool += mutatedAst.toString(grammar = Some(grammar))
      for ((pgm, i) <- pool.toList.zipWithIndex)
        dumpFile(data = pgm, filename = s"$DEDUP_LOG_DIR/${i + 1}.js")

    // get string of mutated AST
    val mutated = mutatedAst.toString(grammar = Some(grammar))

    // dump the mutated ECMAScript program
    for (filename <- config.out)
      dumpFile(
        name = "the mutated ECMAScript program",
        data = mutated,
        filename = filename,
      )

    mutated

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the mutated ECMAScript program to a given path.",
    ),
    (
      "mutator",
      StrOption((c, s) =>
        c.builder = s match
          case "random" => RandomMutator
          case _        => RandomMutator,
      ),
      "select a mutator (default: random).",
    ),
    (
      "untilValid",
      BoolOption(c => c.untilValid = true),
      "repeat until the mutated program becomes valid.",
    ),
    (
      "dedup",
      BoolOption(c => c.dedup = true),
      "repeat until the mutated program satisfies the pattern.",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var builder: Mutator.Builder = RandomMutator,
    var untilValid: Boolean = false,
    var dedup: Boolean = false,
  )
}
