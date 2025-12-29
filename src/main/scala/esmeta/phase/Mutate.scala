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
    import Coverage.*, Target.*

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

    val target = config.target match
      case Some(str) =>
        val (rawId, rawCond) = str.splitAt(str.indexOf(":"))
        val (prefix, suffix) = ("Branch[", "]")
        val tid = if (rawId.startsWith(prefix) && rawId.endsWith(suffix)) {
          rawId.substring(prefix.length, rawId.length - 1).toInt
        } else raise("invalid target")
        val tcond = rawCond.drop(1) match
          case "T" => true; case "F" => false
          case _   => raise("invalid target")
        val touchedCondViews = cov.run(code).touchedCondViews
        val targetCondView = touchedCondViews.filter { (cv, targets) =>
          cv.cond.branch.id == tid && cv.cond.cond == tcond
        }.headOption
        targetCondView match
          case Some((cv, targets)) =>
            if (config.debug)
              val codeStr = code.toString
              println(s"Original Code: $codeStr")
              val localized = targets.map {
                case Normal(loc) => s"Normal: ${loc.getString(codeStr)}"
                case BuiltinThis(thisArg) => s"Builtin this: ${thisArg}"
                case BuiltinArg(arg, idx) => s"Builtin ${idx}th arg: ${arg}"
              }
              println("Localized:")
              localized.foreach(target => println(s"- $target"))
            Some((cv, cov))
          case None => println("Given program doesn't cover target"); None
      case None => None

    val mutator = config.builder(using cfg)
    var blocked = Set[String]()
    var iter = 0
    val trial = config.trial.getOrElse(Int.MaxValue)

    val startTime = System.currentTimeMillis
    def elapsed = System.currentTimeMillis - startTime
    def timeout = config.duration.fold(false)(_ * 1000 < elapsed)

    // get a mutated code
    var mutatedCode = mutator(code, target).code

    // get string of mutated code
    def mutated = mutatedCode.toString

    iter += 1

    def coversFlipped(code: Code): Boolean = target match
      case Some((cv, _)) =>
        val flipped = cv.neg
        if (config.debug) println(s"[iter: $iter] $code")
        val covered = cov.run(code).touchedCondViews.keySet.contains(flipped)
        if (covered) println(s"Covered $flipped with $iter iters")
        covered
      case None => false

    // repeat until the mutated program is valid and covers target
    config.target match
      case Some(str) =>
        while (!(ValidityChecker(mutated) && coversFlipped(mutatedCode))) {
          while (blocked.contains(mutated))
            mutatedCode = mutator(code, target).code
          iter += 1
          blocked += mutated
          if (iter >= trial) raise(s"Failed to cover $str after $iter iters")
          if (timeout) raise(s"Failed to cover $str after $iter iters")
        }
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
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "repeat until the mutated program covers the targeted branch.",
    ),
    (
      "trial",
      NumOption((c, n) => c.trial = Some(n)),
      "set the number of trials (default: INF).",
    ),
    (
      "duration",
      NumOption((c, k) => c.duration = Some(k)),
      "set the maximum duration for mutation (default: INF).",
    ),
    (
      "debug",
      BoolOption((c, b) => c.debug = b),
      "turn on debugging mode.",
    ),
  )
  class Config(
    var out: Option[String] = None,
    var builder: CFG ?=> Mutator = RandomMutator(),
    var target: Option[String] = None,
    var trial: Option[Int] = None,
    var duration: Option[Int] = None,
    var debug: Boolean = false,
  )
}
