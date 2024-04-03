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
import esmeta.injector.NormalTag

case object MinifyFuzz extends Phase[CFG, Coverage] {
  val name = "minify-fuzz"
  val help = "generate ECMAScript programs for fuzzing minifier"

  // TODO(@hyp3rflow): refactor this
  val counter = AtomicLong(0)
  val logDir: String = s"$MINIFY_FUZZ_LOG_DIR/fuzz-$dateStr"
  val symlink: String = s"$MINIFY_FUZZ_LOG_DIR/recent"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage =
    config.seed.foreach(setSeed)
    val graph = GrammarGraph(cfg.grammar)
    import graph.*

    mkdir(logDir, remove = true)
    createSymlink(symlink, logDir, overwrite = true)
    dumpFile(getSeed, s"$logDir/seed")

    val cov = Fuzzer(
      cfg = cfg,
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      beforeCheck = { (finalState, code) =>
        val injector = ReturnInjector(cfg, finalState)
        injector.exitTag match
          case NormalTag =>
            val returns = injector.assertions
            for (ret <- returns.par) {
              val wrapped = s"const k = (() => {\n$code\n$ret\n})();\n"
              Minifier.minifySwc(wrapped) match
                case Failure(exception) => println(exception)
                case Success(minified) => {
                  val injected =
                    Injector.replaceBody(cfg, wrapped, minified, true, false)
                  JSEngine.runNode(injected, Some(1000)) match
                    case Success(v) => println(s"pass $v")
                    case Failure(exception) =>
                      log(wrapped, minified, injected, exception.toString)
                }
            }
          case _ =>
      },
    ).result

    for (dirname <- config.out) cov.dumpToWithDetail(dirname)

    cov

  def log(
    original: String,
    minified: String,
    injected: String,
    exception: String,
  ) = {
    val count = counter.incrementAndGet()
    val dirpath = s"$logDir/$count"
    mkdir(dirpath)
    dumpFile(original, s"$dirpath/original.js")
    dumpFile(minified, s"$dirpath/minified.js")
    dumpFile(injected, s"$dirpath/injected.js")
    dumpFile(exception, s"$dirpath/reason")
  }

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
      "init",
      StrOption((c, s) => c.init = Some(s)),
      "explicitly use the given init pool",
    ),
    (
      "sens",
      BoolOption(c => c.useSens = true),
      "turn on node/branch coverage sensitivity",
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
    var cp: Boolean = false,
    var init: Option[String] = None,
    var useSens: Boolean = false,
  )
}
