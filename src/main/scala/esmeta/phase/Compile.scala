package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.spec.*
import esmeta.spec.util.*
import esmeta.util.*

/** `compile` phase */
case object Compile extends Phase[Spec, Spec] {
  val name = "compile"
  val help = "compiles specification to a IR functions"
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): Spec = {
    spec.program // compile
    if (LOG) spec.dumpProgram(COMPILE_LOG_DIR)
    spec
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
