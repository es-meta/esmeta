package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.dump.*

/** `dump-visualizer` phase */
case object DumpVisualizer extends Phase[CFG, Unit] {
  val name = "dump-visualizer"
  val help =
    "dumps the resources required by the visualizer. (for internal use)"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    DumpSecIdToFuncInfo(cfg)
    DumpStepToNodeId(cfg)
    DumpNodeIdToProgId(cfg)
    DumpNodeIdToTest262(cfg)
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
