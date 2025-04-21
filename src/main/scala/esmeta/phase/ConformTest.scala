package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.injector.*
import esmeta.es.util.injector.ConformTest.*
import esmeta.interpreter.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Try, Success, Failure}

/** `conform-test` phase */
case object ConformTest extends Phase[CFG, Unit] {
  val name = "conform-test"
  val help =
    "performs conformance test for an JavaScript engine or a transpiler"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    val baseDir = getFirstFilename(cmdConfig, "conform-test")
    val scriptDir = s"$baseDir/minimal"
    val assertionDir = s"$baseDir/minimal-assertion"
    val names = getNames(baseDir)
    val transpiler = config.transpiler
    val logDir = s"$baseDir/test/$transpiler"
    mkdir(logDir)
    val scripts = getScripts(scriptDir)
    val totalCount = scripts.size
    val prefix = globalClearingCode
    val progress = ProgressBar(
      "conformance test for scripts",
      scripts.zipWithIndex,
      getName = { case ((x, _), _) => x.name },
      concurrent = ConcurrentPolicy.Auto,
      errorHandler = (e, _, name) => println(s"Error: $name: $e"),
    )
    for (pair <- progress) {
      val (Script(code, name), i) = pair
      val codeWithUseStrict = USE_STRICT + LINE_SEP + code
      val assertion = readFile(s"$assertionDir/$name.js")
      val isNormal = assertion.split(LINE_SEP).head.contains("[EXIT] normal")
      if (nodeEngine.isValid(code)) for {
        transpiled <- JSTrans.transpileSrv(
          codeWithUseStrict,
          Some(transpiler),
        ) match
          case Success(t) => Some(t)
          case Failure(e) =>
            log(logDir, name, i, codeWithUseStrict, "", "", e.getMessage)
            None
        injected = {
          if (isNormal) List(prefix, transpiled, wrap(assertion))
          else List(assertion, prefix, transpiled)
        }.mkString(LINE_SEP)
        reason <- engine.run(injected) match
          case Success(msg) if msg.isEmpty =>
            if (isNormal) None
            else Some("Transpiled Program Should have thrown an exception")
          case Success(msg) => Some(msg)
          case Failure(e) =>
            if (isNormal)
              Some(
                s"Transpiled Program Should Run Normally\nTranspiled Program Exception: ${e.getMessage}",
              )
            else None
      } log(logDir, name, i, codeWithUseStrict, transpiled, injected, reason)
    }
    val bugCount: Int = bugIndexCounter.get
    println(s"Total: $totalCount, Bugs: $bugCount")
    println(s"Bug rate: ${bugCount.toDouble / totalCount * 100}%")
    config.out.map { out =>
      dumpJson(
        Json.obj(
          "total" -> totalCount.asJson,
          "bugs" -> bugCount.asJson,
          "bugRate" -> (bugCount.toDouble / totalCount * 100).asJson,
        ),
        out,
      )
    }
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // get names of the files of the given directory
  private def getNames(dir: String) =
    listFiles(dir).filter(_.isFile).map(_.getName)

  // get scripts from the given directory
  private def getScripts(dirname: String) = for {
    script <- listFiles(dirname)
    filename = script.getName
    if jsFilter(filename)
    name = removedExt(filename)
    code = readFile(script.getPath).linesIterator
      .filterNot(_.trim.startsWith("//"))
      .mkString("\n")
      .strip
      .drop(USE_STRICT.length)
      .strip
  } yield Script(code, name)

  private def wrap(assertion: String): String = List(
    libHead,
    Injector.header,
    delayHead,
    assertion,
    delayTail,
    libTail,
  ).mkString(LINE_SEP)

  // header and footer for tests
  private lazy val libHead = "(()=>{"
  private lazy val libTail = "})();"
  private lazy val delayHead = "$delay(() => {"
  private lazy val delayTail = "});"

  private lazy val engine = JSEngine.default.get
  private lazy val nodeEngine = JSEngine.Engine.Node

  // get code to clear initial global variables
  private lazy val globalClearingCode: String = {
    val stringKeys = engine
      .run("for (let s in globalThis) print(s);")
      .get
      .split(LINE_SEP)
      .filterNot(_ == "")
    val symbolKeys = engine
      .run(
        "for (let s of Object.getOwnPropertySymbols(globalThis)) if(Object.getOwnPropertyDescriptor(globalThis,s).enumerable) print(s.toString());",
      )
      .get
      .split(LINE_SEP)
      .filterNot(_ == "")
      .map(_.replace("Symbol(", "[").replace(")", "]"))
    val globals = stringKeys ++ symbolKeys
    if (globals.isEmpty) ""
    else {
      globals
        .map(v => s"$v: { enumerable: false }")
        .mkString(
          s"\"use strict\"; Object.defineProperties(globalThis , { ",
          ", ",
          s" });$LINE_SEP",
        )
    }
  }

  lazy val bugIndexCounter = AtomicInteger(0)

  def log(
    logDir: String,
    name: String,
    iter: Int,
    original: String,
    transpiled: String,
    injected: String,
    reason: String,
  ): Unit = {
    val iterName = s"minimal-$name-iter-$iter"
    val count = bugIndexCounter.incrementAndGet()
    val dirpath = s"$logDir/$count"
    mkdir(dirpath)
    dumpFile(transpiled, s"$dirpath/transpiled.js")
    dumpFile(injected, s"$dirpath/injected.js")
    dumpFile(original, s"$dirpath/original.js")
    dumpFile(reason, s"$dirpath/reason")
  }

  val defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
    (
      "transpiler",
      StrOption((c, s) => c.transpiler = s),
      "transpiler to use (default: swc).",
    ),
    (
      "debug",
      NumOption((c, k) => c.debug = Some(k)),
      "set the debug level",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var transpiler: String = "swc",
    var debug: Option[Int] = Some(0),
  )
}
