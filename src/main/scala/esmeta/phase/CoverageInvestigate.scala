package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.es.util.USE_STRICT
import esmeta.es.Script
import esmeta.js.minifier.Minifier
import esmeta.injector.Injector
import scala.util.*
import java.util.concurrent.atomic.AtomicLong
import esmeta.util.SystemUtils.*

case object CoverageInvestigate extends Phase[CFG, Unit] {
  val name = "coverage-investigate"
  val help = "investigate newly covered parts by checking additional scripts."

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    val cov_dir = getFirstFilename(cmdConfig, "coverage-investigate")
    val new_js_dir = getSecondFilename(cmdConfig, "coverage-investigate")

    val cov = Coverage.fromLogSimpl(cov_dir, cfg)
    println(s"Coverage restored from $cov_dir")

    val res = (for {
      jsFile <- listFiles(new_js_dir)
      name = jsFile.getName
      code = readFile(jsFile.getPath).drop(USE_STRICT.length).strip
      script = Script(code, name)
      (finalSt, updated, covered) = cov.runAndCheck(script)
    } yield {
      (name, covered)
    }).sortBy(_._2)

    res.foreach((name, covered) => {
      println(f"$name%50s: ${if (covered) "alive" else "dead"}")
    })

    for (filename <- config.out)
      dumpJson(
        data = res,
        filename = filename,
      )
    ()

  val defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
  )

  class Config(
    var out: Option[String] = None,
  )
}
