package esmeta.phase

import esmeta.*
import esmeta.extractor.Extractor
import esmeta.lang.Step
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
    val spec = Extractor(config.target)
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

    // TODO dump algorithms
    // dumpDir(
    //   name = "algorithms",
    //   iterable = spec.algorithms,
    //   dirname = s"$EXTRACT_LOG_DIR/algos",
    //   getName = algo => s"${algo.normalizedName}.algo",
    // )

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
        } warn(e.getMessage)

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
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "repl",
      BoolOption(c => c.repl = true),
      "use a REPL for metalanguage parser.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var log: Boolean = false,
    var repl: Boolean = false,
  )
}
