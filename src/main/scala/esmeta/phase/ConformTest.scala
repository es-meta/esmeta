package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.injector.Injector
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import io.circe.Json
import io.circe.syntax.*
import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.{ConcurrentLinkedQueue, TimeUnit, TimeoutException}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

/** `conform-test` phase */
case object ConformTest extends Phase[CFG, Unit] {
  val name = "conform-test"
  val help = "injects and performs conformance tests on JavaScript engines."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    val scriptDir = File(getFirstFilename(cmdConfig, name)).getAbsoluteFile
    if (!scriptDir.isDirectory)
      raise(s"conform-test requires a directory of ECMAScript files: $scriptDir")

    val engines = EngineSpec.resolve(config.engine)
    val workDir = Files.createTempDirectory("esmeta-conform-work-")
    val results = try {
      val tests = inject(cfg, scriptDir, workDir, config.timeLimit)
      engines.map(runEngine(workDir.toString, tests, _, config.timeLimit))
    } finally rmdir(workDir.toString)

    for (filename <- config.out)
      dumpJson(reportJson(scriptDir.getPath, results), filename)
  }

  /** inject source programs once in a temporary workspace */
  private def inject(
    cfg: CFG,
    scriptDir: File,
    workDir: Path,
    timeLimit: Option[Int],
  ): List[TestInput] = {
    val injectConfig = Inject.Config(
      defs = true,
      timeLimit = timeLimit,
    )
    val (injected, total) = Inject.injectFiles(
      cfg,
      scriptDir.getPath,
      injectConfig,
    )
    if (injected.isEmpty)
      raise(s"No injectable ECMAScript programs in $scriptDir")

    val injectedDir = workDir.resolve("minimal-injected").toString
    mkdir(injectedDir)
    val tests = injected.map { (filename, source) =>
      dumpFile(source, s"$injectedDir/$filename")
      TestInput(
        filename,
        readFile(File(scriptDir, filename).getPath),
        source,
      )
    }
    println(
      s"Injected ${injected.size}/$total ECMAScript program(s), " +
      s"skipped ${total - injected.size}.",
    )
    tests
  }

  // -------------------------------------------------------------------------
  // conformance tests
  // -------------------------------------------------------------------------
  private case class TestInput(
    name: String,
    source: String,
    injected: String,
  ) {
    val expected: String =
      injected.linesIterator
        .find(_.startsWith("// [EXIT] "))
        .flatMap(
          _.stripPrefix("// [EXIT] ").trim match
            case "normal"  => Some("normal")
            case "timeout" => Some("timeout")
            case tag if tag.startsWith("throw-error:") =>
              val name = tag.stripPrefix("throw-error:").trim
              Option.when(name.nonEmpty)(s"throw-error: $name")
            case tag if tag.nonEmpty => Some("throw")
            case _                   => None,
        )
        .getOrElse(raise(s"Invalid injected artifact: $name"))
    val async: Boolean = source.contains("async") || source.contains("Promise")
  }

  private def runEngine(
    baseDir: String,
    tests: List[TestInput],
    engine: EngineSpec,
    timeLimit: Option[Int],
  ): EngineResult = {
    val logDir = s"$baseDir/test/${engine.id}"
    mkdir(logDir, remove = true)
    val prefix = globalClearingCode(engine, timeLimit)
    val bugCounter = AtomicInteger(0)
    val failures = ConcurrentLinkedQueue[FailedRun]()
    val prepared = tests.map(test => test -> prepare(test, prefix))
    val preparedByName = prepared.map { (test, injected) =>
      test.name -> (test, injected)
    }.toMap
    def record(run: FailedRun): Unit = {
      failures.add(run)
      log(logDir, bugCounter, run)
    }
    val progress = ProgressBar(
      s"conformance test with ${engine.id}",
      prepared,
      getName = (entry, _) => entry._1.name,
      concurrent = ConcurrentPolicy.Auto,
      errorHandler = (error, summary, testName) => {
        summary.fail.add(testName)
        val (test, injected) = preparedByName(testName)
        record(
          FailedRun(
            test,
            injected,
            Failure(
              "infrastructure-error",
              test.expected,
              "unknown",
              "",
              describe(error),
            ),
          ),
        )
      },
    )

    for ((test, injected) <- progress) {
      classify(test, execute(engine, injected, timeLimit)) match
        case Outcome.Pass | Outcome.Skip =>
        case Outcome.Fail(failure) =>
          record(FailedRun(test, injected, failure))
    }

    val bugs = groupBugs(failures.iterator.asScala.toVector)
    println(s"${engine.id}: ${bugs.size}/${tests.size} bugs")
    EngineResult(engine, bugs)
  }

  private def prepare(test: TestInput, prefix: String): String =
    List(prefix, test.injected).filter(_.nonEmpty).mkString(LINE_SEP)

  // -------------------------------------------------------------------------
  // engines
  // -------------------------------------------------------------------------
  private case class EngineSpec(
    id: String,
    path: Path,
  ) {
    def command(script: Path): List[String] =
      if (id == "quickjs") List(path.toString, "--script", script.toString)
      else List(path.toString, script.toString)
  }
  private object EngineSpec {
    val baseDir: Path = Path.of(System.getProperty("user.home"), ".jsvu", "bin")

    private val definitions = List(
      "v8" -> List("v8"),
      "javascriptcore" -> List("jsc", "javascriptcore"),
      "graaljs" -> List("graaljs"),
      "spidermonkey" -> List("sm", "spidermonkey"),
      "xs" -> List("xs"),
      "quickjs" -> List("qjs", "quickjs"),
    )

    private def installed(
      definition: (String, List[String]),
    ): Option[EngineSpec] = {
      val (id, aliases) = definition
      aliases
        .map(baseDir.resolve)
        .find(Files.isExecutable(_))
        .map(EngineSpec(id, _))
    }

    def resolve(name: String): List[EngineSpec] =
      val normalized = name.toLowerCase
      if (normalized == "all") {
        val engines = definitions.flatMap(installed)
        val installedIds = engines.map(_.id).toSet
        val missing = definitions.map(_._1).filterNot(installedIds)
        if (missing.nonEmpty)
          println(s"Not installed: ${missing.mkString(", ")}")
        if (engines.isEmpty)
          raise(s"No JavaScript engines are installed in $baseDir")
        engines
      } else {
        val definition = definitions
          .find { (id, aliases) =>
            id == normalized || aliases.contains(normalized)
          }
          .getOrElse(
            raise(
              s"Unknown JavaScript engine: $name " +
              s"(available: all, ${definitions.map(_._1).mkString(", ")})",
            ),
          )
        List(
          installed(definition).getOrElse(
            raise(
              s"JavaScript engine is not installed: " +
              s"${definition._1} in $baseDir",
            ),
          ),
        )
      }
  }

  private val errorName = """\b([A-Za-z]*Error)(?=[:\r\n])""".r

  private case class Execution(
    timedOut: Boolean,
    exitCode: Int,
    stdout: String,
    stderr: String,
  ) {
    def concrete: String =
      if (timedOut) "timeout"
      else if (exitCode == 0) "normal"
      else
        errorName
          .findFirstMatchIn(stdout + LINE_SEP + stderr)
          .map(result => s"throw-error: ${result.group(1)}")
          .getOrElse("throw")
  }

  private def execute(
    engine: EngineSpec,
    source: String,
    timeLimit: Option[Int],
  ): Execution = {
    val script = Files.createTempFile("esmeta-conform-", ".js")
    val stdoutFile = Files.createTempFile("esmeta-conform-stdout-", ".log")
    val stderrFile = Files.createTempFile("esmeta-conform-stderr-", ".log")
    var process: java.lang.Process = null
    try {
      Files.writeString(script, source, UTF_8)
      process = ProcessBuilder(engine.command(script)*)
        .directory(File(BASE_DIR))
        .redirectOutput(stdoutFile.toFile)
        .redirectError(stderrFile.toFile)
        .start
      val finished = timeLimit match
        case Some(seconds) => process.waitFor(seconds.toLong, TimeUnit.SECONDS)
        case None => process.waitFor; true
      if (!finished) {
        process.destroyForcibly
        process.waitFor
      }
      Execution(
        timedOut = !finished,
        exitCode = if (finished) process.exitValue else -1,
        stdout = Files.readString(stdoutFile, UTF_8).trim,
        stderr = Files.readString(stderrFile, UTF_8).trim,
      )
    } finally {
      if (process != null && process.isAlive) {
        process.destroyForcibly
        process.waitFor
      }
      Files.deleteIfExists(script)
      Files.deleteIfExists(stdoutFile)
      Files.deleteIfExists(stderrFile)
    }
  }

  private def checkedOutput(
    engine: EngineSpec,
    source: String,
    timeLimit: Option[Int],
  ): String = {
    val result = execute(engine, source, timeLimit)
    if (result.timedOut) throw TimeoutException(engine.id)
    if (result.exitCode != 0)
      throw RuntimeException(List(result.stdout, result.stderr).mkString)
    result.stdout
  }

  // Hide host-specific enumerable globals before running a synthesized test.
  private def globalClearingCode(
    engine: EngineSpec,
    timeLimit: Option[Int],
  ): String = {
    val stringKeys = checkedOutput(
      engine,
      "for (let s in globalThis) print(s);",
      timeLimit,
    ).linesIterator.filter(_.nonEmpty)
    val symbolKeys = checkedOutput(
      engine,
      "for (let s of Object.getOwnPropertySymbols(globalThis)) " +
      "if(Object.getOwnPropertyDescriptor(globalThis,s).enumerable) " +
      "print(s.toString());",
      timeLimit,
    ).linesIterator
      .filter(_.nonEmpty)
      .map(_.replace("Symbol(", "[").replace(")", "]"))
    val globals = (stringKeys ++ symbolKeys).toVector
    if (globals.isEmpty) ""
    else
      globals
        .map(value => s"$value: { enumerable: false }")
        .mkString(
          s"\"use strict\"; Object.defineProperties(globalThis , { ",
          ", ",
          s" });$LINE_SEP",
        )
  }

  // -------------------------------------------------------------------------
  // result classification and logging
  // -------------------------------------------------------------------------
  private val unhandled = "(?i)unhandled.{0,30}(reject|promise)".r

  private def assertionOutput(stdout: String): String =
    stdout.linesIterator
      .filter(_.startsWith(Injector.assertionFailurePrefix))
      .map(_.stripPrefix(Injector.assertionFailurePrefix))
      .mkString(LINE_SEP)

  private def describe(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty) match
      case Some(message) => s"${error.getClass.getName}: $message"
      case None          => error.getClass.getName

  private case class Failure(
    category: String,
    expected: String,
    concrete: String,
    stdout: String,
    stderr: String,
  )
  private enum Outcome {
    case Pass
    case Skip
    case Fail(failure: Failure)
  }
  private case class FailedRun(
    test: TestInput,
    injected: String,
    failure: Failure,
  )
  private case class Bug(program: String, failures: Vector[Failure])
  private case class EngineResult(engine: EngineSpec, bugs: Vector[Bug])

  private def classify(test: TestInput, result: Execution): Outcome = {
    val want = test.expected
    val got = result.concrete
    val output = result.stdout + LINE_SEP + result.stderr
    val assertionFailure = assertionOutput(result.stdout)
    val category =
      if (unhandled.findFirstIn(output).isDefined)
        Some("host-unhandled-rejection" -> true)
      else if (got != want) Some("exit-tag-mismatch" -> false)
      else if (want == "normal" && assertionFailure.nonEmpty)
        Some(
          (if (test.async) "async-assertion-fail"
           else "assertion-fail") -> false,
        )
      else None

    category match
      case None                => Outcome.Pass
      case Some((_, true))     => Outcome.Skip
      case Some((name, false)) =>
        val stdout =
          if (name.endsWith("assertion-fail")) assertionFailure
          else result.stdout
        Outcome.Fail(Failure(name, want, got, stdout, result.stderr))
  }

  private def groupBugs(runs: Vector[FailedRun]): Vector[Bug] =
    runs
      .groupBy(_.test.source)
      .toVector
      .sortBy(_._1)
      .map { (program, grouped) =>
        val failures = grouped
          .map(_.failure)
          .distinctBy(failure =>
            (
              failure.category,
              failure.expected,
              failure.concrete,
              failure.stdout,
              failure.stderr,
            ),
          )
        Bug(program, failures)
      }

  private def log(
    logDir: String,
    counter: AtomicInteger,
    run: FailedRun,
  ): Unit = {
    val dir = s"$logDir/${counter.incrementAndGet}"
    mkdir(dir)
    dumpFile(run.test.source, s"$dir/original.js")
    dumpFile(run.injected, s"$dir/injected.js")
    dumpFile(reason(run.failure), s"$dir/reason")
  }

  private def reason(failure: Failure): String = {
    val lines = Vector.newBuilder[String]
    lines += s"[${failure.category}]"
    lines += s"Expected: ${failure.expected}"
    lines += s"Concrete: ${failure.concrete}"
    if (failure.stdout.nonEmpty) lines += s"stdout: ${failure.stdout}"
    if (failure.stderr.nonEmpty) lines += s"stderr: ${failure.stderr}"
    lines.result.mkString(LINE_SEP)
  }

  private def reportJson(
    input: String,
    results: List[EngineResult],
  ): Json = Json.obj(
    "input" -> input.asJson,
    "engines" -> Json.fromFields(results.map { result =>
      result.engine.id -> Json.obj(
        "engine" -> result.engine.path.toString.asJson,
        "bugs" -> Json.fromValues(result.bugs.map(bugJson)),
      )
    }),
  )

  private def bugJson(bug: Bug): Json = Json.obj(
    "program" -> bug.program.asJson,
    "failures" -> Json.fromValues(bug.failures.map(failureJson)),
  )

  private def failureJson(failure: Failure): Json = {
    val fields = Vector.newBuilder[(String, Json)]
    fields += "category" -> failure.category.asJson
    fields += "expected" -> failure.expected.asJson
    fields += "concrete" -> failure.concrete.asJson
    if (failure.stdout.nonEmpty)
      fields += "stdout" -> truncate(failure.stdout).asJson
    if (failure.stderr.nonEmpty)
      fields += "stderr" -> truncate(failure.stderr).asJson
    Json.fromFields(fields.result)
  }

  private def truncate(text: String): String =
    text.take(1000) + (if (text.length > 1000) "..." else "")

  val defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((config, filename) => config.out = Some(filename)),
      "output JSON file path.",
    ),
    (
      "engine",
      StrOption((config, engine) => config.engine = engine),
      "JavaScript engine to test, or all installed engines (default: all).",
    ),
    (
      "timeout",
      NumOption((config, seconds) => config.timeLimit = Some(seconds)),
      "set the time limit in seconds (default: 10 seconds).",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var engine: String = "all",
    var timeLimit: Option[Int] = Some(10),
  )
}
