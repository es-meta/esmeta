package esmeta.phase

import esmeta.*
import esmeta.extractor.Extractor
import esmeta.lang.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import org.jsoup.nodes.Element

/** `extract` phase */
case object YetCheck extends Phase[Unit, (Int, Int)] {
  val name = "yet-check"
  val help = "checks `yet-step` and `yet-type` in the specification."
  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): (Int, Int) = cmdConfig.targets match {
    case from :: to :: Nil =>
      val verbose = !cmdConfig.silent

      // extract `from` specification
      if (verbose) println(s"Extracting the 'from' specification from: `$from`")
      val fromSpec = Extractor(from)

      // extract `to` specification
      if (verbose) println(s"Extracting the 'to' specification from: `$to`")
      val toSpec = Extractor(to)

      // get diffs between `from` and `to`
      val diffs = Spec.getDiffs(from, to, "spec.html")
      if (verbose) println(s"${diffs.size} total diffs found.")

      // filter target diffs (excluding already `yet` steps in `from`)
      val yetStepsInFrom = getYetSteps(fromSpec)
      val yetStepLines = getLines(yetStepsInFrom)
      val stepDiffs = filterDiffs(diffs, yetStepLines)
      if (verbose)
        println(s"${diffs.size - stepDiffs.size} diffs excluded for steps.")

      // alert newly introduced yet steps in the `to` specification
      val linesForSteps = stepDiffs.flatMap(_.added).toList.sorted
      val yetSteps = for {
        target <- getYetSteps(toSpec)
        if linesForSteps.exists(target.range.contains)
      } yield target

      // filter target type diffs (excluding already `yet` types in `from`)
      val yetTypesInFrom = getYetTypes(fromSpec)
      val yetTypeLines = getLines(yetTypesInFrom)
      val typeDiffs = filterDiffs(diffs, yetTypeLines)
      if (verbose)
        println(s"${diffs.size - typeDiffs.size} diffs excluded for types.")

      // alert newly introduced yet types in the `to` specification
      val linesForTypes = typeDiffs.flatMap(_.added).toList.sorted
      val yetTypes = for {
        target <- getYetTypes(toSpec)
        if linesForTypes.exists(target.range.contains)
      } yield target

      if (config.log) log(yetSteps, yetTypes)
      if (config.gitHubAlert) alert(yetSteps, yetTypes)

      (yetSteps.size, yetTypes.size)

    case _ => raise("`yet-check` phase needs exactly two targets: <from> <to>")
  }

  case class Target[T <: Locational](target: T, range: Range, algo: Algorithm)

  def alert(
    yetSteps: List[Target[Step]],
    yetTypes: List[Target[Type]],
  ): Unit = {
    // alert newly introduced yet steps
    for { Target(step, range, algo) <- yetSteps } yield GitHubAction.println(
      tag = "warning",
      file = Some("spec.html"),
      line = Some(range.start),
      endLine = Some(range.end - 1),
      title = Some("Newly Introduced Unknown Step"),
      message = Some(s"""
        |This step in ${algo.name} cannot be understood by ESMeta.
        |Type check for this algorithm will not be performed after this line.
        |""".stripMargin.replace('\n', ' ').trim),
    )

    // alert newly introduced yet types
    for { Target(ty, range, algo) <- yetTypes } yield GitHubAction.println(
      tag = "warning",
      file = Some("spec.html"),
      line = Some(range.start),
      endLine = Some(range.end - 1),
      title = Some("Newly Introduced Unknown Type"),
      message = Some(
        s"""The type `$ty` used in ${algo.name}
        |is unknown type which ESMeta cannot understand.
        |This type will be treated as bottom (⊥) type initially
        |but will be joined with argument types when the algorithm is called.
        |""".stripMargin.replace('\n', ' ').trim,
      ),
    )
  }

  def getYetSteps(spec: Spec): List[Target[Step]] =
    getYets(spec, _.elem, _.incompleteSteps)

  def getYetTypes(spec: Spec): List[Target[Type]] =
    getYets(spec, _.headElem, _.yetTypes)

  def getLines(targets: List[Target[_]]): List[Int] =
    targets.flatMap(_.range).toSet.toList.sorted

  def filterDiffs(diffs: List[Git.Diff], excluded: List[Int]): List[Git.Diff] =
    diffs.filter(diff => !excluded.exists(diff.removed.contains))

  def getYets[E, T <: Locational](
    spec: Spec,
    getElem: Algorithm => Element,
    getTarget: Algorithm => List[T],
  ): List[Target[T]] = for {
    algo <- spec.algorithms
    startLine = getElem(algo).sourceRange.start.lineNumber
    target <- getTarget(algo)
    loc <- target.loc.toList
    range = startLine + loc.start.line - 1 until startLine + loc.end.line
  } yield Target(target, range, algo)

  def log(yetSteps: List[Target[Step]], yetTypes: List[Target[Type]]): Unit = {
    dumpJson(
      name = "not yet supported steps",
      data = yetSteps.map(target => {
        Map(
          "step" -> target.target.toString(detail = false, location = false),
          "algorithm" -> target.algo.name,
          "lines" -> target.range.toString,
        )
      }),
      filename = s"$YET_CHECK_LOG_DIR/yet-steps.json",
    )
    dumpJson(
      name = "not yet supported types",
      data = yetTypes.map(target => {
        Map(
          "type" -> target.target.toString,
          "algorithm" -> target.algo.name,
          "lines" -> target.range.toString,
        )
      }),
      filename = s"$YET_CHECK_LOG_DIR/yet-types.json",
    )
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "github-alert",
      BoolOption(_.gitHubAlert = _),
      "turn on GitHub alert mode.",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var gitHubAlert: Boolean = false,
  )
}
