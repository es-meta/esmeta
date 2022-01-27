package esmeta.phase

import esmeta.*
import esmeta.spec.*
import esmeta.spec.Utils.*
import esmeta.util.*
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*

/** `extract` phase */
case object Extract extends Phase[Unit, Spec] {
  val name = "extract"
  val help = "extracts specification model from ECMA-262 (spec.html)."
  def apply(
    unit: Unit,
    globalConfig: GlobalConfig,
    config: Config,
  ): Spec = {
    val spec = config.json match {
      case Some(filename) => ???
      case None =>
        val filename = getFirstFilename(globalConfig, "extract")
        val content = readFile(filename)
        Parser.parseSpec(content)
    }

    // logging mode
    if (config.log) {
      mkdir(EXTRACT_LOG_DIR)

      val yets = spec.incompleteSteps
      dumpFile(
        name = "not yet supported steps",
        data = yets
          .map(_.toString(detail = false, location = false))
          .sorted
          .mkString("\n"),
        filename = s"$EXTRACT_LOG_DIR/yets",
      )

      dumpFile(
        name = "the summary of extracted specification",
        data = spec,
        filename = s"$EXTRACT_LOG_DIR/summary",
      )

      // Statistics
      if (config.stat) {
        // TODO : too verbose
        val filename = getFirstFilename(globalConfig, "extract")
        val content = readFile(filename)
        val docu = content.toHtml

        for {
          algo <- spec.algorithms
        } {
          val targets = List(
            ("Algo_Total", 1),
            ("Step_Total", algo.steps.length),
            ("Algo_Pass", if algo.complete then 1 else 0),
            ("Step_Pass", algo.completeSteps.length),
          )
          SpecStats.addAlgo(docu)(algo, targets)
        }

        // log Statistics
        mkdir(s"$EXTRACT_LOG_DIR/stat")

        val algoStr = SpecStats.getAllStr(spec, docu.body, "Algo")
        dumpFile(
          name = "the summary of algorithms",
          data = algoStr,
          filename = s"$EXTRACT_LOG_DIR/stat/algo-summary",
        )

        val stepStr = SpecStats.getAllStr(spec, docu.body, "Step")
        dumpFile(
          name = "the summary of algorithm steps",
          data = stepStr,
          filename = s"$EXTRACT_LOG_DIR/stat/step-summary",
        )

        val stepStatStr = (for {
          (name, count) <- spec.stats(0).toList.sortBy(_._2)
        } yield f"$count%-5d $name").mkString(LINE_SEP)
        dumpFile(
          name = "the summary of spec step-stat",
          data = stepStatStr,
          filename = s"$EXTRACT_LOG_DIR/stat/step-stat-summary",
        )

        val exprStatStr = (for {
          (name, count) <- spec.stats(1).toList.sortBy(_._2)
        } yield f"$count%-5d $name").mkString(LINE_SEP)
        dumpFile(
          name = "the summary of spec expr-stat",
          data = exprStatStr,
          filename = s"$EXTRACT_LOG_DIR/stat/expr-stat-summary",
        )

        val condStatStr = (for {
          (name, count) <- spec.stats(2).toList.sortBy(_._2)
        } yield f"$count%-5d $name").mkString(LINE_SEP)
        dumpFile(
          name = "the summary of spec expr-stat",
          data = condStatStr,
          filename = s"$EXTRACT_LOG_DIR/stat/cond-stat-summary",
        )
      }
    }
    spec
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "json",
      StrOption((c, s) => c.json = Some(s)),
      "load specification from JSON.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "stat",
      BoolOption(c => {
        c.log = true
        c.stat = true
      }),
      "turn on stat mode.",
    ),
  )
  case class Config(
    var json: Option[String] = None,
    var log: Boolean = false,
    var stat: Boolean = false,
  )
}
