package esmeta.phase

import esmeta.*
import esmeta.ir.Program
import esmeta.compiler.Compiler
import esmeta.rocq.{RocqGenerator, RocqPartialGenerator}
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `rocqgen` phase */
case object RocqGen extends Phase[Program, Unit] {
  val name = "rocqgen"
  val help = "generates Rocq files for a specification."
  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    ???
    ()
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "partial",
      BoolOption(_.partial = _),
      "dump sample Rocq files and their source IR ASTs.",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var partial: Boolean = false,
  )
}
