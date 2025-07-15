package esmeta.phase

import esmeta.*
import esmeta.extractor.Extractor
import esmeta.lang.Step
import esmeta.lang.util.ParserForEval.{getParseCount, getCacheCount}
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.io.StdIn.readLine

/** `extract` phase */
case object Extract extends Phase[Unit, Spec] {
  val name = "extract"
  val help = "extracts specification model from ECMA-262 (spec.html)."
  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Spec = if (!config.repl) {
    lazy val spec = Extractor(config.target, config.eval)
    if (config.eval)
      time("extracting specification", spec)
      println(f"- # of actual parsing: $getParseCount%,d")
      println(f"- # of using cached result: $getCacheCount%,d")
    if (config.log) log(spec)
    spec
  } else {
    runREPL
    Spec()
  }

  // logging mode
  private def log(spec: Spec): Unit = {
    mkdir(EXTRACT_LOG_DIR)

    val yetSteps = spec.incompleteSteps
    dumpFile(
      name = "not yet supported steps",
      data = yetSteps
        .map(_.toString(detail = false, location = false))
        .sorted
        .mkString(LINE_SEP),
      filename = s"$EXTRACT_LOG_DIR/yet-steps",
    )

    val yetTypes = spec.yetTypes
    dumpFile(
      name = "not yet parsed types",
      data = yetTypes
        .map(_.toString)
        .sorted
        .mkString(LINE_SEP),
      filename = s"$EXTRACT_LOG_DIR/yet-types",
    )

    dumpFile("grammar", spec.grammar, s"$EXTRACT_LOG_DIR/grammar")

    dumpDir(
      name = "algorithms",
      iterable = spec.algorithms,
      dirname = s"$EXTRACT_LOG_DIR/algos",
      getName = algo => s"${algo.normalizedName}.algo",
    )

    dumpFile(
      name = "algorithms whose string form is not equal to the original prose",
      data = spec.algorithms
        .filter(algo => algo.normalizedCode != algo.body.toString)
        .map(algo => s"$EXTRACT_LOG_DIR/algos/${algo.normalizedName}.algo")
        .sorted
        .mkString(LINE_SEP),
      filename = s"$EXTRACT_LOG_DIR/yet-equal-algos",
    )

    dumpFile(
      name = "the summary of extracted specification",
      data = spec,
      filename = s"$EXTRACT_LOG_DIR/summary",
    )

    spec.stats.dumpTo(s"$EXTRACT_LOG_DIR/stat")
  }

  // run REPL
  private val replWelcomeMessage =
    """Welcome to REPL for metalanguage parser .
      |Please input any metalanguage step as an input of the parser.
      |If you want to exit, please type `q` ro `quit`.""".stripMargin
  def runREPL: Unit =
    println(replWelcomeMessage)
    var keep = true
    while (keep) stop("parser> ") match
      case "q" | "quit" => keep = false
      case input =>
        for {
          e <- getError(println(Step.from(input)))
        } warn(getMessage(e))

  // stop and read user message
  private def stop(msg: String): String = { print(msg); readLine }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target git version of ECMA-262 (default: current version).",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "eval",
      BoolOption(_.eval = _),
      "evaluate the extractor.",
    ),
    (
      "repl",
      BoolOption(_.repl = _),
      "use a REPL for metalanguage parser.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var log: Boolean = false,
    var eval: Boolean = false,
    var repl: Boolean = false,
  )
}
