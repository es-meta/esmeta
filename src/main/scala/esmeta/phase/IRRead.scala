package esmeta.phase

import esmeta.*
import esmeta.ir.Program
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `ir-read` phase */
case object IRRead extends Phase[Unit, Program] {
  val name = "ir-read"
  val help = "read a single IR-ES file."

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Program = run(config, getFirstFilename(cmdConfig, this.name))

  def run(config: Config, filename: String): Program =
    Program.fromFile(filename)

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = Nil
  case class Config()
}
