package esmeta.phase

import esmeta.*
import esmeta.spec.Spec
import esmeta.interp.*
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
    val content = readFile(filename)
    val program = Program.from(content)
    val st = State(program.cfg)
    new Interp(st).fixpoint
    st
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
