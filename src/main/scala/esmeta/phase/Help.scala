package esmeta.phase

import esmeta.{ESMeta, GlobalConfig}

/** `help` phase */
case object Help extends Phase[Unit, Unit]:
  val name = "help"
  val help = "shows help messages."
  def apply(
    unit: Unit,
    globalConfig: GlobalConfig,
    config: Config,
  ): Unit = println(ESMeta.help)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = Nil
  case class Config()
