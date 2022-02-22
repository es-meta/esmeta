package esmeta.phase

import esmeta.*
import esmeta.spec.Spec
import esmeta.interp.*
import esmeta.cfg.util.Builder
import esmeta.ir.*
import esmeta.ir.util.*
import esmeta.util.SystemUtils.*

/** `ir-eval` phase */
case object IREval extends Phase[Unit, State] {
  val name = "ir-eval"
  val help = "evaluates an IR file."
  def apply(
    unit: Unit,
    globalConfig: GlobalConfig,
    config: Config,
  ): State = {
    val filename = getFirstFilename(globalConfig, "extract")
    val program = Program.fromFile(filename)
    val cfg = Builder(program)
    val st = State(cfg)
    new Interp(st).fixpoint
    st
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
