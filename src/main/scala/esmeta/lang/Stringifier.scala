package esmeta.lang

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for language */
case class Stringifier(detail: Boolean) {
  // elements
  given elemRule: Rule[LangElem] = (app, elem) =>
    elem match {
      case elem: Program => programRule(app, elem)
      case elem: Block   => blockRule(app, elem)
      case elem: Step    => stepRule(app, elem)
    }

  // TODO programs
  given programRule: Rule[Program] = _ >> _.block

  // TODO blocks
  given blockRule: Rule[Block] = (app, block) =>
    app.wrap("", "")(block match {
      case Order(steps) =>
        steps.foreach(app :> "1. " >> _)
      case Unorder(steps) =>
        steps.foreach(app :> "* " >> _)
      case Figure(lines) =>
        app :> "<figure>"
        app.wrap("", "")(lines.map(app :> _))
        app >> "</figure>"
    })

  // TODO steps
  given stepRule: Rule[Step] = (app, step) =>
    step match {
      case Yet(str, block) =>
        app >> str
        block.fold(app)(app >> _)
    }
}
