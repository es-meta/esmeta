package esmeta.phase

import esmeta.*
import esmeta.extractor.Extractor
import esmeta.lang.*
import esmeta.lang.util.ParserForEval.{getParseCount, getCacheCount}
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.nio.file.Paths
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
    if (config.strict && config.log) warnInvalidPath(config.allowedYets)
    if (config.eval)
      time("extracting specification", spec)
      println(f"- # of actual parsing: $getParseCount%,d")
      println(f"- # of using cached result: $getCacheCount%,d")
    if (config.log) log(spec)
    if (config.strict) checkStrict(spec, config)
    spec
  } else {
    runREPL
    Spec()
  }

  private def warnInvalidPath(path: Option[String]): Unit = for {
    p <- path
  } do {
    val parent = Paths.get(EXTRACT_LOG_DIR).toAbsolutePath.normalize
    val child = Paths.get(p).toAbsolutePath.normalize

    if (child startsWith parent)
      warn(
        "`allowed-yets` is set to a path under the log directory; `-extract:log` option may overwrite the given `-extract:allowed-yets` file.",
      )
  }

  private def checkStrict(spec: Spec, config: Config): Unit = {
    // TODO warn unused elements in ignore file
    val ignoreMap = config.allowedYets match
      case None =>
        warn(
          s"no ignore file for `allowed-yet-types`; using default allowlist. (default: none)",
        )
        Map.empty
      case Some(value) => readJson[Map[String, List[String]]](value)

    // check shape of json
    val isWellShaped =
      ignoreMap.keySet.subsetOf(Set("yet-steps", "yet-conds", "yet-types"))
    if (!isWellShaped) {
      warn(
        s"invalid ignore file for `allowed-yet-types`: expected keys are `yet-steps`, `yet-conds`, and `yet-types`, but given ${ignoreMap.keySet}.",
      )
    }

    val disallowedYetTypes = {
      val ignoredTypes = ignoreMap.get("yet-types").getOrElse(List.empty)
      spec.yetTypes
        .map(_.toString)
        .filterNot(ignoredTypes.contains)
    }

    val disAllowedYetConds = {
      val ignoredConds = ignoreMap.get("yet-conds").getOrElse(List.empty)
      spec.yetConds
        .map(_.toString(detail = true, location = false))
        .filterNot(ignoredConds.contains)
    }

    val disallowedYetSteps = {
      val ignoredSteps = ignoreMap.get("yet-steps").getOrElse(List.empty)
      (ManualInfo.compileRule("inst").keySet ++
      spec.yetSteps.map(
        _.toString(detail = true, location = false),
      ))
        .filterNot(ignoredSteps.contains)
    }

    val disallowed =
      disallowedYetTypes.nonEmpty || disallowedYetSteps.nonEmpty || disAllowedYetConds.nonEmpty

    if (disallowed) {
      raise(
        s"extracting failed in strict extract mode: found ${spec.yetTypes.length} yet-types, ${spec.yetSteps.length} yet-steps and ${spec.yetConds.length} yet-conds. see result of -extract:log for details.",
      )
    }
  }

  // logging mode
  private def log(spec: Spec): Unit = {
    mkdir(EXTRACT_LOG_DIR)

    dumpJson(
      name = "not yet supported steps",
      data = spec.yetSteps
        .map(_.toString(detail = true, location = false))
        .sorted,
      filename = s"$EXTRACT_LOG_DIR/yet-steps.json",
    )

    dumpJson(
      name = "not yet supported conditions",
      data = spec.yetConds
        .map(_.toString(detail = true, location = false))
        .sorted,
      filename = s"$EXTRACT_LOG_DIR/yet-conds.json",
    )

    val yetTypes = spec.yetTypes
    dumpJson(
      name = "not yet parsed types",
      data = spec.yetTypes
        .map(_.toString)
        .sorted,
      filename = s"$EXTRACT_LOG_DIR/yet-types.json",
    )

    dumpJson(
      name =
        "not yet supported steps, not yet supported conditions, and not yet parsed types in one file",
      data = Map(
        "yet-steps" -> spec.yetSteps
          .map(_.toString(detail = true, location = false))
          .sorted,
        "yet-conds" -> spec.yetConds
          .map(_.toString(detail = true, location = false))
          .sorted,
        "yet-types" -> spec.yetTypes
          .map(_.toString)
          .sorted,
      ),
      filename = s"$EXTRACT_LOG_DIR/total-yets.json",
    )

    dumpFile("grammar", spec.grammar, s"$EXTRACT_LOG_DIR/grammar")

    dumpDir(
      name = "algorithms",
      iterable = spec.algorithms,
      dirname = s"$EXTRACT_LOG_DIR/algos",
      getName = algo => s"${algo.normalizedName}.algo",
    )

    dumpJson(
      name = "algorithms whose string form is not equal to the original prose",
      data = spec.algorithms
        .filter(algo => algo.normalizedCode != algo.body.toString)
        .map(algo => s"$EXTRACT_LOG_DIR/algos/${algo.normalizedName}.algo")
        .sorted,
      filename = s"$EXTRACT_LOG_DIR/yet-equal-algos.json",
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
    (
      "strict",
      BoolOption(_.strict = _),
      "turn on strict parsing mode, which makes extractor fail when any 'yet-step' or 'yet-type`. (default: false)",
    ),
    (
      "allowed-yets",
      StrOption((c, s) => c.allowedYets = Some(s)),
      "set a file containing allowed `yet`s (default: none).",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var log: Boolean = false,
    var eval: Boolean = false,
    var repl: Boolean = false,
    var strict: Boolean = false,
    var allowedYets: Option[String] = None,
  )
}
