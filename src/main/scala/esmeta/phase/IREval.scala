package esmeta.phase

import esmeta.*
import esmeta.interp.*
import esmeta.ir.*
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
    val cfg = program.toCFG
    val st = State(cfg)
    new Interp(st).fixpoint
    st
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
