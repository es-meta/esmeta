package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.{NotSupported => NSError, InterpreterError}
import esmeta.injector.Injector
import esmeta.interpreter.Interpreter
import esmeta.es.*
import esmeta.state.*
import esmeta.test262.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import java.io.File
import java.util.concurrent.TimeoutException

/** `inject` phase */
case object Inject extends Phase[CFG, String] {
  val name = "inject"
  val help = "injects assertions to check final state of an ECMAScript file."

  private def injectFile(cfg: CFG, filename: String, config: Config): String =
    Injector
      .fromFile(cfg, filename, config.log, config.timeLimit, config.instrument)
      .toString(detail = config.defs)

  private[phase] def injectFiles(
    cfg: CFG,
    dirname: String,
    config: Config,
  ): (List[(String, String)], List[File]) = {
    val files = listFiles(dirname)
      .filter(f => f.isFile && jsFilter(f.getName))
      .sortBy(_.getName)
    val (skipped, injected) = files.partitionMap { f =>
      try Right(f.getName -> injectFile(cfg, f.getPath, config))
      catch {
        case _: InterpreterError | _: NSError | _: TimeoutException => Left(f)
      }
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
      val total = injected.size + skipped.size
      config.out match
        case Some(dirname) =>
          mkdir(dirname, remove = true)
          for ((filename, source) <- injected)
            dumpFile(source, s"$dirname/$filename")
          s"Injected ${injected.size}/$total ECMAScript program(s), " +
          s"skipped ${skipped.size}."
        case None =>
          injected.map(_._2).mkString(LINE_SEP + LINE_SEP)
    } else {
      val injected = injectFile(cfg, path, config)

      // dump the assertion-injected ECMAScript program
      for (filename <- config.out)
        dumpFile(
          name = "an assertion-injected ECMAScript program",
          data = injected,
          filename = filename,
        )

      injected
    }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "defs",
      BoolOption(_.defs = _),
      "prepend definitions of helpers for assertions.",
    ),
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump assertion-injected ECMAScript program(s) to a given path.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "instrument",
      BoolOption(_.instrument = _),
      "instrument expression evaluation order.",
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
    var defs: Boolean = false,
    var out: Option[String] = None,
    var log: Boolean = false,
    var instrument: Boolean = false,
    var batch: Boolean = false,
    var timeLimit: Option[Int] = Some(10),
  )
}
