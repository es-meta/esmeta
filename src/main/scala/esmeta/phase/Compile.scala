package esmeta.phase

import esmeta.*
import esmeta.ir.Program
import esmeta.ir.util.*
import esmeta.util.*
import esmeta.spec.*
import esmeta.spec.util.*
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
    val program = Compiler(spec)
    if (LOG) program.dumpTo(COMPILE_LOG_DIR)
    program
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
