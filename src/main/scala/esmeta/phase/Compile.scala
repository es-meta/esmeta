package esmeta.phase

import esmeta.*
import esmeta.spec.*
import esmeta.spec.util.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.cfg.*

/** `compile` phase */
case object Compile extends Phase[Spec, CFG] {
  val name = "compile"
  val help = "compiles specification to a control-flow graph"
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): CFG = {
    val cfg = spec.toCFG
    if (LOG) {
      mkdir(COMPILE_LOG_DIR)
      for {
        (name, func) <- cfg.fnameMap
        filename = s"$COMPILE_LOG_DIR/${name.replace("/", "")}.cfg"
      } dumpFile(func, filename)
    }
    if (config.dot) {
      mkdir(CFG_LOG_DIR)
      // dump dot format
      cfg.funcs.foreach(f => {
        val name = s"${CFG_LOG_DIR}/${f.head.name}"
        dumpFile(f.toDot, s"$name.dot")
      })

      if (config.pdf) {
        ??? // dump pdf format
      }
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
