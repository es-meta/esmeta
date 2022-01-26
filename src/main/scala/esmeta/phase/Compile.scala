package esmeta.phase

import esmeta.*
import esmeta.spec.*
import esmeta.cfg.*

/** `compile` phase */
case object Compile extends Phase[Spec, CFG] {
  val name = "compile"
  val help = "compiles specification to a control-flow graph"
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): CFG = Compiler(spec)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
