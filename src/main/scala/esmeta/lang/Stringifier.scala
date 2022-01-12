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
      case elem: Stmt => stmtRule(app, elem)
      case _          => ???
    }

  // TODO statements
  given stmtRule: Rule[Stmt] = (app, stmt) => ???
}
