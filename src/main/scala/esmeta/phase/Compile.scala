package esmeta.phase

import esmeta.*
import esmeta.ir.Program
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `compile` phase */
case object Compile extends Phase[Spec, Program] {
  val name = "compile"
  val help = "compiles a specification to an IR program."
  def apply(
    spec: Spec,
    cmdConfig: CommandConfig,
    config: Config,
  ): Program = {
    val program = spec.toIR

    // logging mode
    if (config.log)
      program.dumpTo(COMPILE_LOG_DIR)
      dumpFile(
        name = "yet expressions",
        data = program.yets
          .map(_.toString(detail = false, location = false))
          .sorted
          .mkString(LINE_SEP),
        filename = s"$COMPILE_LOG_DIR/yets",
      )

    program
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var log: Boolean = false,
  )
}
