package esmeta.phase

import esmeta.*
import esmeta.util.{BoolOption, StrOption}

/** Backward-compatible alias for the standalone-IR `rocq` pipeline. */
case object RocqGenIR extends Phase[Unit, Unit] {
  val name = "rocqgen-ir"
  val help = "generates Rocq files for standalone IR programs."

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    RocqGen(
      (),
      cmdConfig,
      RocqGen.Config(config.out, config.proofObligations),
    )

  def defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption(_.out = _),
      "output directory (default: logs/rocq-ir).",
    ),
    (
      "proof-obligations",
      BoolOption(_.proofObligations = _),
      "generate path-sensitive proof obligations for IR assertions.",
    ),
  )

  case class Config(
    var out: String = ROCQ_IR_LOG_DIR,
    var proofObligations: Boolean = false,
  )
}
