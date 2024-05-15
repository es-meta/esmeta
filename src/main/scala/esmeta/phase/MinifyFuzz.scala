package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.spec.util.GrammarGraph
import esmeta.es.util.fuzzer.Fuzzer
import esmeta.js.minifier.Minifier
import esmeta.injector.Injector
import scala.util.*
import esmeta.js.JSEngine
import esmeta.injector.ReturnInjector
import esmeta.interpreter.Interpreter
import java.util.concurrent.atomic.AtomicLong
import scala.collection.parallel.CollectionConverters._
import esmeta.util.SystemUtils.*
import esmeta.es.util.ValidityChecker
import esmeta.es.util.fuzzer.MinifyFuzzer

case object MinifyFuzz extends Phase[CFG, Coverage] {
  val name = "minify-fuzz"
  val help = "generate ECMAScript programs for fuzzing minifier"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage =
    config.seed.foreach(setSeed)
    val graph = GrammarGraph(cfg.grammar)
    import graph.*

    val cov = MinifyFuzzer(
      cfg = cfg,
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      kFs = config.kFs,
      cp = config.cp,
      init = config.init,
    )

    for (dirname <- config.out) cov.dumpToWithDetail(dirname)

    cov

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "log-interval",
      NumOption((c, k) => c.logInterval = Some(k)),
      "turn on logging mode and set logging interval (default: 600 seconds).",
    ),
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the generated ECMAScript programs to a given directory.",
    ),
    (
      "debug",
      NumOption((c, k) =>
        if (k < 0 || k > 2) error("invalid debug level: please set 0 to 2")
        else c.debug = k,
      ),
      "turn on deug mode with level (0: no-debug, 1: partial, 2: all)",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: 1 second).",
    ),
    (
      "trial",
      NumOption((c, k) => c.trial = Some(k)),
      "set the number of trials (default: INF).",
    ),
    (
      "duration",
      NumOption((c, k) => c.duration = Some(k)),
      "set the maximum duration for fuzzing (default: INF)",
    ),
    (
      "seed",
      NumOption((c, k) => c.seed = Some(k)),
      "set the specific seed for the random number generator (default: None).",
    ),
    (
      "cp",
      BoolOption(c => c.cp = true),
      "turn on the call-path mode (default: false) (meaningful if k-fs > 0).",
    ),
    (
      "k-fs",
      NumOption((c, k) => c.kFs = k),
      "set the k-value for feature sensitivity (default: 0).",
    ),
    (
      "init",
      StrOption((c, s) => c.init = Some(s)),
      "explicitly use the given init pool",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var logInterval: Option[Int] = Some(600), // 1 minute
    var out: Option[String] = None,
    var debug: Int = 0,
    var timeLimit: Option[Int] = Some(1),
    var trial: Option[Int] = None,
    var duration: Option[Int] = None,
    var seed: Option[Int] = None,
    var init: Option[String] = None,
    var kFs: Int = 0,
    var cp: Boolean = false,
  )
}
