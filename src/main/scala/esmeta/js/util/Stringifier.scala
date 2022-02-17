package esmeta.js.util

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.js.*

/** stringifier for JavaScript */
class Stringifier(detail: Boolean, location: Boolean) {
  // elements
  given elemRule: Rule[JsElem] = (app, elem) =>
    elem match {
      case elem: Ast => astRule(app, elem)
    }

  // abstract syntax tree (AST) values
  given astRule: Rule[Ast] = (app, ast) =>
    ast match
      case Syntactic(name, args, rhsIdx, children) =>
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule()
        app >> "|" >> name >> "|"
        if (!args.isEmpty) app >> "[" >> args >> "]"
        app >> "<" >> rhsIdx >> ">"
        given Rule[Option[Ast]] = optionRule("<none>")
        if (detail) app.wrap("(", ")")(children.map(app :> _ >> ",")) else app
      case Lexical(name, str) =>
        app >> "|" >> name >> "|(" >> str >> ")"
}
