package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.ir.Program
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `build-cfg` phase */
case object BuildCFG extends Phase[Program, CFG] {
  val name = "build-cfg"
  val help = "builds a control-flow graph (CFG) from an IR program."
  def apply(
    program: Program,
    globalConfig: GlobalConfig,
    config: Config,
  ): CFG = {
    // build cfg
    val cfg = program.toCFG

    // print dot
    if (config.dot) {
      if (config.pdf && !isNormalExit("dot -V")) {
        println("Dot is not installed!")
        config.pdf = false
      }
      for { f <- cfg.funcs } f.dumpDot(CFG_LOG_DIR, pdf = config.pdf)
    }

    cfg
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
