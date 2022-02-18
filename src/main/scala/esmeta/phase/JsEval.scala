package esmeta.phase

import esmeta.*
import esmeta.spec.Spec
import esmeta.interp.*
import esmeta.js.*
import esmeta.js.util.*

/** `js-eval` phase */
case object JsEval extends Phase[Spec, State] {
  val name = "js-eval"
  val help = "evaluates a JavaScript file."
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): State = {
    val st = State(spec.program.cfg)
    new Interp(st).fixpoint
    st
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
