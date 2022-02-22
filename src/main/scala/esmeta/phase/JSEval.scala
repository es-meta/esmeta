package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfg.util.*
import esmeta.interp.*
import esmeta.util.SystemUtils.*
import esmeta.js.*
import esmeta.js.util.{Parser => JSParser}
import esmeta.js.builtin.*

/** `js-eval` phase */
case object JSEval extends Phase[CFG, State] {
  val name = "js-eval"
  val help = "evaluates a JavaScript file."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): State = {
    // get source text
    val filename = getSecondFilename(globalConfig, "js-eval")
    val st = Initialize(cfg, readFile(filename))
    val jsParser = JSParser(cfg.grammar)
    new Interp(st, Some(jsParser)).fixpoint
    st
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
