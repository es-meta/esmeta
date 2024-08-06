package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.state.*
import esmeta.test262.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.util.fuzzer.TracerInjector

/** `injectTracer` phase */
case object InjectTracer extends Phase[CFG, String] {
  val name = "inject-tracer"
  val help = "injects tracers to check execution order of an ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String =
    val filename = getFirstFilename(cmdConfig, this.name)
    val script = cfg.scriptParser.fromFile(filename)
    val injector = TracerInjector(using cfg)
    val injected = injector.walk(script).toString(grammar = Some(cfg.grammar))
    // dump the tracer-injected ECMAScript program
    for (filename <- config.out)
      dumpFile(
        name = "an tracer-injected ECMAScript program",
        data = injected,
        filename = filename,
      )
    injected

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump an assertion-injected ECMAScript program to a given path.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var log: Boolean = false,
  )
}
