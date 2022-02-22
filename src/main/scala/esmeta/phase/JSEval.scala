package esmeta.phase

import esmeta.*
import esmeta.spec.Spec
import esmeta.interp.*
import esmeta.util.SystemUtils.*
import esmeta.js.*
import esmeta.js.util.{Parser => JSParser}
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
    // get source text
    val filename = getSecondFilename(globalConfig, "js-eval")
    // TODO refactoring to mechanized spec
    val (st, typeModel) = Initialize(spec, readFile(filename))
    val jsParser = JSParser(spec.grammar)
    new Interp(
      st,
      typeModel = Some(typeModel),
      jsParser = Some(jsParser),
    ).fixpoint
    st
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
