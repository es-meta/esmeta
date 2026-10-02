package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.{NotSupported => NSError, InterpreterError}
import esmeta.injector.{
  ConformTest => InjectedTest,
  Injector,
  InteractionOracle,
}
import esmeta.interpreter.Interpreter
import esmeta.es.*
import esmeta.state.*
import esmeta.test262.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import java.io.File
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeoutException
import scala.collection.mutable.{Set => MSet}

/** `inject` phase */
case object Inject extends Phase[CFG, String] {
  val name = "inject"
  val help =
    "injects final-state assertions and optional interaction-based tests."

  private def injectFile(
    cfg: CFG,
    filename: String,
    config: Config,
    ownedCoverage: Option[Set[(Int, Boolean)]],
  ): List[InjectedTest] =
    Injector.tests(
      cfg,
      readFile(filename),
      interaction = config.interaction,
      log = config.log,
      timeLimit = config.timeLimit,
      filename = Some(filename),
      ownedCoverage = ownedCoverage,
    )

  private def nameTests(
    filename: String,
    tests: List[InjectedTest],
    used: MSet[String],
  ): List[(String, InjectedTest)] = tests.zipWithIndex.map { (test, index) =>
    val name =
      if (index == 0) filename
      else if (
        tests.size == 2 && !used(
          s"${filename.stripSuffix(".js")}.interaction.js",
        )
      )
        s"${filename.stripSuffix(".js")}.interaction.js"
      else
        LazyList
          .from(1)
          .map(i => s"${filename.stripSuffix(".js")}.interaction-$i.js")
          .find(name => !used(name))
          .get
    used += name
    name -> test
  }

  private[phase] def injectFiles(
    cfg: CFG,
    dirname: String,
    config: Config,
  ): (List[(String, InjectedTest)], List[File]) = {
    val files = listFiles(dirname)
      .filter(f => f.isFile && jsFilter(f.getName))
      .sortBy(_.getName)
    val ownedCoverage =
      if (config.interaction)
        InteractionOracle.loadOwnedCoverage(cfg, File(dirname).getAbsoluteFile)
      else Map.empty[String, Set[(Int, Boolean)]]
    val completed = new ConcurrentHashMap[String, List[InjectedTest]]()
    val bar = ProgressBar(
      msg = "injecting assertions",
      iterable = files,
      // logging keeps one thread so that the logs stay in order
      concurrent =
        if (!config.log)
          ConcurrentPolicy.Fixed(Runtime.getRuntime.availableProcessors)
        else ConcurrentPolicy.Single,
    )
    bar.foreach { f =>
      try {
        val tests = injectFile(
          cfg,
          f.getPath,
          config,
          ownedCoverage.get(f.getCanonicalPath),
        )
        completed.put(f.getName, tests)
      } catch {
        case _: InterpreterError | _: NSError | _: TimeoutException =>
      }
    }
    val (skipped, success) =
      files.partition(f => !completed.containsKey(f.getName))
    val used = MSet.from(files.map(_.getName))
    val injected = success.flatMap { f =>
      nameTests(f.getName, completed.get(f.getName), used)
    }
    (injected, skipped)
  }

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String =
    val path = getFirstFilename(cmdConfig, this.name)
    if (config.batch) {
      val (injected, skipped) = injectFiles(cfg, path, config)
      val total = listFiles(path).count(f => f.isFile && jsFilter(f.getName))
      config.out match
        case Some(dirname) =>
          mkdir(dirname, remove = true)
          for ((filename, test) <- injected)
            dumpFile(test.toString(detail = config.defs), s"$dirname/$filename")
          s"Injected ${injected.size} test(s) from $total ECMAScript program(s), " +
          s"skipped ${skipped.size} input(s)."
        case None =>
          injected
            .map(_._2.toString(detail = config.defs))
            .mkString(LINE_SEP + LINE_SEP)
    } else {
      val file = File(path).getAbsoluteFile
      val ownedCoverage =
        if (config.interaction)
          InteractionOracle
            .loadOwnedCoverage(cfg, file.getParentFile)
            .get(file.getCanonicalPath)
        else None
      val tests = injectFile(cfg, path, config, ownedCoverage)
      val named =
        nameTests(config.out.getOrElse(file.getName), tests, MSet.empty)
      val rendered =
        named.map((name, test) => name -> test.toString(detail = config.defs))
      if (config.out.nonEmpty)
        for ((filename, code) <- rendered)
          dumpFile(
            name = "an assertion-injected ECMAScript program",
            data = code,
            filename = filename,
          )
      if (rendered.size == 1) rendered.head._2
      else
        rendered
          .map((name, code) => s"// $name\n$code")
          .mkString(LINE_SEP + LINE_SEP)
    }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "interaction",
      BoolOption(_.interaction = _),
      "add interaction-based tests alongside final-state tests (default: false).",
    ),
    (
      "defs",
      BoolOption(_.defs = _),
      "prepend definitions of helpers for assertions.",
    ),
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output file (an interaction variant uses .interaction.js; multiple variants use .interaction-N.js), or directory with -inject:batch.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "batch",
      BoolOption(_.batch = _),
      "inject assertions into all JavaScript files in a target directory, " +
      "skipping not-supported files.",
    ),
    (
      "timeout",
      NumOption((config, seconds) => config.timeLimit = Some(seconds)),
      "set the injection time limit in seconds (default: 10 seconds).",
    ),
  )
  case class Config(
    var interaction: Boolean = false,
    var defs: Boolean = false,
    var out: Option[String] = None,
    var log: Boolean = false,
    var batch: Boolean = false,
    var timeLimit: Option[Int] = Some(10),
  )
}
