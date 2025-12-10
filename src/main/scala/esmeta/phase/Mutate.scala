package esmeta.phase

import esmeta.*
import esmeta.analyzer.paramflow.*
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.fuzzer.mutator.*
import esmeta.parser.ESParser
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*

/** `mutate` phase */
case object Mutate extends Phase[CFG, String] {
  val name = "mutate"
  val help = "mutates an ECMAScript program."
  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): String =
    import Coverage.*

    val jsonProtocol = new JsonProtocol(cfg)
    import jsonProtocol.{*, given}

    val grammar = cfg.grammar
    val filename = getFirstFilename(cmdConfig, this.name)
    val code =
      if (filename.endsWith(".js")) Code.Normal(readFile(filename))
      else if (filename.endsWith(".json")) readJson[Code](filename)
      else raise("invalid filename")

    val analyzer = ParamFlowAnalyzer(cfg)
    analyzer.analyze
    val cov = Coverage(cfg, analyzer = Some(analyzer))

    val target = config.untilCovered match
      case Some(str) =>
        val (rawId, rawCond) = str.splitAt(str.indexOf(":"))
        val (prefix, suffix) = ("Branch[", "]")
        val tid = if (rawId.startsWith(prefix) && rawId.endsWith(suffix)) {
          rawId.substring(prefix.length, rawId.length - 1).toInt
        } else raise("invalid target")
        val tcond = rawCond.drop(1) match
          case "T" => true; case "F" => false
          case _   => raise("invalid target")
        val touchedCondViews = cov.run(code).touchedCondViews.keySet
        touchedCondViews
          .filter(cv => cv.cond.branch.id == tid && cv.cond.cond == tcond)
          .headOption
          .map((_, cov))
      case None => None

    val mutator = config.builder(using cfg)
    var iter = 0

    // get a mutated code
    var mutatedCode = mutator(code, target).code

    // get string of mutated code
    def mutated = mutatedCode.toString

    iter += 1

    def isFlipped(code: Code): Boolean = target match
      case Some((cv, _)) =>
        cov.run(code).touchedCondViews.keySet.contains(cv.neg)
      case None => false

    // repeat until the mutated program is valid and covers target
    config.untilCovered match
      case Some(str) =>
        while (!(ValidityChecker(mutated) && isFlipped(mutatedCode))) {
          mutatedCode = mutator(code, target).code; iter += 1
          if (iter >= config.trial.getOrElse(10000))
            raise(s"Failed to cover $str after $iter iters")
        }
        println(s"Covered with $iter iters")
      case None => ()

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
          case "RandomMutator"     => RandomMutator()
          case "SpecStringMutator" => SpecStringMutator()
          case "TargetMutator"     => TargetMutator()
          case "StatementInserter" => StatementInserter()
          case "Remover"           => Remover()
          case _                   => RandomMutator(),
      ),
      "select a mutator (default: RandomMutator).",
    ),
    (
      "untilCovered",
      StrOption((c, s) => c.untilCovered = Some(s)),
      "repeat until the mutated program covers the targeted branch.",
    ),
    (
      "trial",
      NumOption((c, n) => c.trial = Some(n)),
      "set the number of trials (default: 10000).",
    ),
  )
  class Config(
    var out: Option[String] = None,
    var builder: CFG ?=> Mutator = RandomMutator(),
    var untilCovered: Option[String] = None,
    var trial: Option[Int] = None,
  )
}
