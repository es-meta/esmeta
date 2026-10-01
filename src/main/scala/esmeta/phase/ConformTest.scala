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
      raise(
        s"conform-test requires a directory of ECMAScript files: $scriptDir",
      )

    val logDir = Option.when(config.log) {
      val base = s"$LOG_DIR/conform-test"
      val dir = s"$base/conform-$dateStr"
      mkdir(dir, remove = true)
      createSymLink(s"$base/recent", dir, overwrite = true)
      dir
    }
    val engines = EngineSpec.resolve(config.engine)
    val workDir = Files.createTempDirectory("esmeta-conform-work-")
    val (tests, results, divergences, injectionMs, conformMs) =
      try {
        val start = System.nanoTime()
        val (tests, skipped) = inject(cfg, scriptDir, workDir, config)
        val injectionMs = (System.nanoTime() - start) / 1e6
        val conformStart = System.nanoTime()
        val results =
          engines.map(runEngine(workDir.toString, tests, _, config.timeLimit))
        val divergences = differential(skipped, engines, config.timeLimit)
        val conformMs = (System.nanoTime() - conformStart) / 1e6
        (tests, results, divergences, injectionMs, conformMs)
      } finally rmdir(workDir.toString)

    val report = reportJson(
      scriptDir.getPath,
      tests.size,
      results,
      divergences,
      Option.when(config.log)((injectionMs, conformMs)),
    )
    val outputs = config.out.toList ++ logDir.map(_ + "/conform.json")
    for (filename <- outputs.distinct)
      dumpJson(report, filename)
    for (dir <- logDir) {
      val summary =
        s"Input: ${scriptDir.getPath}\n" +
        s"Interaction: ${config.interaction}\n" +
        s"Tests: ${tests.size}\n" +
        f"Injection: $injectionMs%.3f ms\n" +
        f"Conform-test (engines + differential): $conformMs%.3f ms\n" +
        results.map { result =>
          s"${result.engine.id}: ${result.bugs.size}/${tests.size} failures\n"
        }.mkString +
        s"Differential divergences: ${divergences.size}\n" +
        (if (config.interaction)
           esmeta.injector.Injector.InteractionStats.summary
         else "")
      dumpFile(summary, s"$dir/summary")
    }
  }

  /** a program without an oracle, where a minority of engines stands apart */
  private case class Divergence(
    program: String,
    odd: List[String],
    tags: Map[String, String],
  )

  private def differential(
    skipped: List[(String, String)],
    engines: List[EngineSpec],
    timeLimit: Option[Int],
  ): List[Divergence] = {
    if (skipped.isEmpty || engines.sizeIs < 3) Nil
    else {
      val bar = ProgressBar(
        "differential test on programs without assertions",
        skipped,
        getName = (entry, _) => entry._1,
        concurrent = ConcurrentPolicy.Auto,
        errorHandler = (_, summary, name) => summary.fail.add(name),
      )
      val found = ConcurrentLinkedQueue[Divergence]()
      bar.foreach { (_, source) =>
        // NOTE: no global-hiding prefix, since these carry no assertions
        val runs = engines.map(e => e.id -> execute(e, source, timeLimit))
        val noisy = runs.exists { (_, r) =>
          unhandled.findFirstIn(r.stdout + LINE_SEP + r.stderr).isDefined
        }
        val tags = runs.map((id, r) => id -> r.concrete).toMap
        val grouped = tags.groupMap(_._2)(_._1)
        // an engine apart from the majority may have a bug (JEST's decision)
        val majority = grouped.values.find(_.size * 2 > engines.size)
        if (!noisy) for {
          agreed <- majority
          odd = tags.keys.filterNot(agreed.toSet).toList.sorted
          if odd.nonEmpty
        } found.add(Divergence(source, odd, tags))
      }
      val divergences = found.iterator.asScala.toList.sortBy(_.program)
      println(
        s"${divergences.size}/${skipped.size} programs divide the engines",
      )
      divergences
    }
  }

  /** inject source programs once in a temporary workspace */
  private def inject(
    cfg: CFG,
    scriptDir: File,
    workDir: Path,
    config: Config,
  ): (List[TestInput], List[(String, String)]) = {
    val injectConfig = Inject.Config(
      defs = true,
      timeLimit = config.timeLimit,
      interaction = config.interaction,
    )
    esmeta.injector.Injector.InteractionStats.reset()
    val (injected, skippedFiles) = Inject.injectFiles(
      cfg,
      scriptDir.getPath,
      injectConfig,
    )
    if (injected.isEmpty)
      raise(s"No injectable ECMAScript programs in $scriptDir")

    val injectedDir = workDir.resolve("minimal-injected").toString
    mkdir(injectedDir)
    val tests = injected.map { (filename, test) =>
      val source = test.toString(detail = injectConfig.defs)
      val original = File(scriptDir, filename)
      dumpFile(source, s"$injectedDir/$filename")
      TestInput(
        filename,
        if (original.isFile) readFile(original.getPath) else test.script,
        source,
        test.async,
      )
    }
    val skipped = skippedFiles.map(f => f.getName -> readFile(f.getPath))
    val total =
      listFiles(scriptDir.getPath).count(f => f.isFile && jsFilter(f.getName))
    println(
      s"Injected ${injected.size} test(s) from $total ECMAScript program(s), " +
      s"skipped ${skipped.size} input(s).",
    )
    if (config.interaction)
      print(esmeta.injector.Injector.InteractionStats.summary)
    (tests, skipped)
  }

  // -------------------------------------------------------------------------
  // conformance tests
  // -------------------------------------------------------------------------
  private case class TestInput(
    name: String,
    source: String,
    injected: String,
    async: Boolean,
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

    progress.foreach { (test, injected) =>
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

  private val errorName = """\b([A-Za-z]*Error)(?=[:\r\n]|$)""".r

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

  /** forcibly terminate a process and every process spawned by it */
  private def destroyProcessTree(process: java.lang.Process): Unit =
    val descendants =
      process.toHandle.descendants.iterator.asScala.toVector.reverse
    descendants.foreach(_.destroyForcibly())
    process.destroyForcibly()
    process.waitFor()
    descendants.filter(_.isAlive).foreach(_.destroyForcibly())

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
        case None          => process.waitFor; true
      if (!finished) destroyProcessTree(process)
      Execution(
        timedOut = !finished,
        exitCode = if (finished) process.exitValue else -1,
        stdout = Files.readString(stdoutFile, UTF_8).trim,
        stderr = Files.readString(stderrFile, UTF_8).trim,
      )
    } finally {
      if (process != null && process.isAlive) destroyProcessTree(process)
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
  private case class Bug(
    program: String,
    failures: Vector[Failure],
    names: Vector[String] = Vector.empty,
  )
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
      case None            => Outcome.Pass
      case Some((_, true)) => Outcome.Skip
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
        Bug(program, failures, grouped.map(_.test.name).distinct.sorted)
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
    tests: Int,
    results: List[EngineResult],
    divergences: List[Divergence],
    timings: Option[(Double, Double)],
  ): Json = Json
    .obj(
      "input" -> input.asJson,
      "tests" -> tests.asJson,
      "engines" -> Json.fromFields(results.map { result =>
        result.engine.id -> Json.obj(
          "engine" -> result.engine.path.toString.asJson,
          "bugs" -> Json.fromValues(result.bugs.map(bugJson)),
        )
      }),
      "divergences" -> Json.fromValues(divergences.map { d =>
        Json.obj(
          "program" -> d.program.asJson,
          "odd" -> d.odd.asJson,
          "tags" -> Json.fromFields(
            d.tags.toList.sorted.map((k, v) => k -> v.asJson),
          ),
        )
      }),
    )
    .mapObject { fields =>
      timings.fold(fields) { (injectionMs, conformMs) =>
        fields
          .add("injectionMs", injectionMs.asJson)
          .add("conformMs", conformMs.asJson)
      }
    }

  private def bugJson(bug: Bug): Json = Json.obj(
    "program" -> bug.program.asJson,
    "names" -> bug.names.asJson,
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
      "interaction",
      BoolOption(_.interaction = _),
      "add interaction-based tests alongside final-state tests (default: false).",
    ),
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
    (
      "log",
      BoolOption(_.log = _),
      "dump results, timings, and summary under logs/conform-test.",
    ),
  )
  case class Config(
    var interaction: Boolean = false,
    var out: Option[String] = None,
    var engine: String = "all",
    var timeLimit: Option[Int] = Some(10),
    var log: Boolean = false,
  )
}
