package esmeta.phase

import scala.util.*
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.SystemUtils.getFirstFilename
import esmeta.interpreter.Interpreter
import esmeta.util.SystemUtils.readFile
import esmeta.es.util.delta.DeltaDebugger
import esmeta.injector.Injector
import esmeta.js.JSEngine
import esmeta.js.minifier.Minifier
import esmeta.util.BaseUtils.error
import esmeta.injector.ReturnInjector
import esmeta.es.util.fuzzer.MinifyTesterResult
import esmeta.injector.NormalTag
import esmeta.util.SystemUtils.*
import scala.collection.parallel.CollectionConverters._
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap
import scala.collection.concurrent.TrieMap
import io.circe.syntax._
import esmeta.es.util.fuzzer.MinifyTester
import esmeta.es.util.fuzzer.MinifyTesterConfig

case object DeltaDebug extends Phase[CFG, String] {
  val name = "delta-debugger"

  val help = "delta-debugs ECMAScript program to minimize buggy program"

  // i am sorry
  var _config: Option[Config] = None

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): String =
    _config = Some(config)
    val minifyTester =
      MinifyTester(cfg, MinifyTesterConfig(debugLevel = config.debug))
    // TODO: fix this after separating silent option
    val result = if (config.multiple) {
      val count = AtomicInteger(0)
      var minimalMap: TrieMap[String, String] = new TrieMap
      val files = (for {
        path <- cmdConfig.targets
        file <- walkTree(path)
        filename = file.toString
        if jsFilter(filename)
      } yield filename)
      files.par.foreach { filename =>
        minimalMap.put(filename, minifyTester.dd(readFile(filename)))
        info("delta-debug", s"${count.incrementAndGet}/${files.size} completed")
      }
      log("delta-debug", s"minimal size: ${minimalMap.size}")
      minimalMap.asJson.toString
    } else {
      val filename = getFirstFilename(cmdConfig, this.name)
      val originalCode = readFile(filename)
      minifyTester.dd(originalCode)
    }
    // TODO(@hyp3rflow): silent option removes both phase header and output log
    println(result); result

  private def log(label: String, description: String) =
    if (_config.get.debug > 1) println(s"[$label] $description")

  private def info(label: String, description: String) =
    if (_config.get.debug > 0) System.err.println(s"[$label] $description")

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "checker",
      StrOption((c, v) => c.checker = v),
      "select checker",
    ),
    (
      "multiple",
      BoolOption(c => c.multiple = true),
      "delta-debug multiple programs",
    ),
    (
      "debug",
      NumOption((c, k) =>
        if (k < 0 || k > 2) error("invalid debug level: please set 0 to 2")
        else c.debug = k,
      ),
      "turn on debug mode with level (0: no-debug, 1: informative, 2: all)",
    ),
  )
  case class Config(
    var checker: String = "minifier",
    var multiple: Boolean = false,
    var debug: Int = 0,
  )
}
