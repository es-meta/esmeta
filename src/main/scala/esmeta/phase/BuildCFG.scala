package esmeta.phase

import esmeta.*
import esmeta.spec.Spec
import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `build-cfg` phase */
case object BuildCFG extends Phase[Spec, Spec] {
  val name = "build-cfg"
  val help = "builds a CFG from compiled specification"
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): Spec = {
    val cfg = spec.program.cfg

    if (config.dot) {
      mkdir(CFG_LOG_DIR)
      // dump dot format
      cfg.funcs.foreach(f => {
        val name = s"${CFG_LOG_DIR}/${f.ir.name.replace("/", "")}"
        dumpFile(f.toDot, s"$name.dot")
      })

      if (config.pdf) {
        ??? // dump pdf format
      }
    }

    spec
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "dot",
      BoolOption(c => c.dot = true),
      "dump the cfg in a dot format.",
    ),
    (
      "pdf",
      BoolOption(c => { c.dot = true; c.pdf = true }),
      "dump the cfg in a dot and pdf format.",
    ),
  )
  case class Config(
    var dot: Boolean = false,
    var pdf: Boolean = false,
  )
}
