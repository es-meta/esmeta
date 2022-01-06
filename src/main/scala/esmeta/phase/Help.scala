package esmeta.phase

import esmeta.{ESMeta, ESMetaConfig}

// Help phase
case object Help extends Phase[Unit, Unit]:
  case class Config()
  val name = "help"
  val help = "shows help messages."
  def apply(
    unit: Unit,
    esmetaConfig: ESMetaConfig,
    config: Config,
  ): Unit = println(ESMeta.help)
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = Nil
