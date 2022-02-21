package esmeta.phase

import esmeta.*
import esmeta.spec.Spec
import esmeta.interp.*
import esmeta.js.*
import esmeta.js.util.*
import esmeta.js.builtin.*

/** `js-eval` phase */
case object JSEval extends Phase[Spec, State] {
  val name = "js-eval"
  val help = "evaluates a JavaScript file."
  def apply(
    spec: Spec,
    globalConfig: GlobalConfig,
    config: Config,
  ): State = {
    val (st, typeModel) = Initialize(spec, "") // XXX get source text
    println(st)
    new Interp(st, typeModel = Some(typeModel)).fixpoint
    st
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
