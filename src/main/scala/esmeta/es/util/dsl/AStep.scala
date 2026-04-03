package esmeta.es.util.dsl

import esmeta.lang.*

/** Annotated Step: a Step paired with the analysis state at its entry point. */
case class AStep(
  step: Step,
  state: Analyzer.AbsState,
  children: List[AStep],
) {

  /** Pretty-print the annotated tree for debugging. */
  def prettyPrint(indent: Int = 0): String =
    val pad = "  " * indent
    val stateStr =
      if (state.isEmpty) "{}"
      else
        state
          .map { case (k, v) => s"$k → ${v.mkString(".")}" }
          .mkString("{", ", ", "}")
    val stepStr = step match
      case BlockStep(_)               => "BlockStep"
      case IfStep(_, _, _, _)         => "IfStep"
      case ForEachStep(_, v, _, _, _) => s"ForEachStep(${v})"
      case RepeatStep(_, _)           => "RepeatStep"
      case LetStep(v, _)              => s"LetStep(${v})"
      case SetStep(r, _)              => s"SetStep(${r})"
      case _                          => step.getClass.getSimpleName
    val self = s"${pad}${stepStr}  $stateStr\n"
    val childStr = children.map(_.prettyPrint(indent + 1)).mkString
    self + childStr
}
