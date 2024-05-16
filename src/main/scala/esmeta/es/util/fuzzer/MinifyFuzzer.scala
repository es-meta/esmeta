package esmeta.es.util.fuzzer

import esmeta.{error => _, *}
import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.es.util.fuzzer.Fuzzer.NO_DEBUG
import esmeta.es.util.Coverage
import esmeta.injector.*
import esmeta.state.State
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.atomic.AtomicLong
import scala.util.*
import scala.collection.parallel.CollectionConverters._
import esmeta.js.minifier.Minifier
import esmeta.js.JSEngine

object MinifyFuzzer {
  def apply(
    cfg: CFG,
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    init: Option[String] = None,
    kFs: Int = 0,
    cp: Boolean = false,
  ): Coverage = new MinifyFuzzer(
    cfg,
    logInterval,
    debug,
    stdOut,
    timeLimit,
    trial,
    duration,
    init,
    kFs,
    cp,
  ).result
}

class MinifyFuzzer(
  cfg: CFG,
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  duration: Option[Int] = None, // `None` denotes no bound
  init: Option[String] = None,
  kFs: Int = 0,
  cp: Boolean = false,
) {
  val counter = AtomicLong(0)
  val logDir: String = s"$MINIFY_FUZZ_LOG_DIR/fuzz-$dateStr"
  val symlink: String = s"$MINIFY_FUZZ_LOG_DIR/recent"

  lazy val result: Coverage =
    Fuzzer(
      cfg = cfg,
      logInterval = logInterval,
      debug = debug,
      timeLimit = timeLimit,
      trial = trial,
      duration = duration,
      init = init,
      kFs = kFs,
      cp = cp,
      beforeCheck = beforeCheck,
      logDir = logDir,
      symlink = symlink,
    ).result

  private def beforeCheck(finalState: State, code: String) =
    val injector = ReturnInjector(cfg, finalState, timeLimit, true)
    injector.exitTag match
      case NormalTag =>
        val returns = injector.assertions
        for (ret <- returns.par) {
          val wrapped = s"const k = (() => {\n$code\n$ret\n})();\n"
          Minifier.minifySwc(wrapped) match
            case Failure(exception) => println(exception)
            case Success(minified) => {
              val injected =
                Injector.replaceBody(
                  cfg,
                  wrapped,
                  minified,
                  defs = true,
                  timeLimit = timeLimit,
                  log = false,
                  ignoreProperties = "\"name\"" :: Nil,
                )
              JSEngine.runNode(injected, Some(1000)) match
                case Success(v) if v.isEmpty => println(s"pass")
                case Success(v) =>
                  log(wrapped, minified, injected, v)
                case Failure(exception) =>
                  log(wrapped, minified, injected, exception.toString)
            }
        }
      case _ =>

  private def log(
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

}
