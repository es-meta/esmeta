package esmeta.phase

import esmeta.*
import esmeta.ir.Program
import esmeta.spec.Spec
import esmeta.util.*

/** `compile` phase */
case object Compile extends Phase[Spec, Program] {
  val name = "compile"
  val help = "compiles a specification to an IR program."
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): Program = {
    val program = spec.toIR

    // logging mode
    if (config.log) program.dumpTo(COMPILE_LOG_DIR)

    program
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode",
    ),
  )
  case class Config(
    var log: Boolean = false,
  )
}
