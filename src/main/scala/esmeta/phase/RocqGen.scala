package esmeta.phase

import esmeta.*
import esmeta.ir.Program
import esmeta.rocq.RocqGenerator
import esmeta.util.BoolOption

/** `rocqgen` phase */
case object RocqGen extends Phase[Program, Unit] {
  val name = "rocqgen"
  val help = "generates Rocq files for a specification."
  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    RocqGenerator(program).dumpTo(ROCQ262_LOG_DIR)

    ()
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    ("log", BoolOption(_.log = _), "turn on logging mode."),
  )
  case class Config(
    var log: Boolean = false,
  )
}
