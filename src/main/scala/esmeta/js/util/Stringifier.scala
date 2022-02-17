package esmeta.js.util

import esmeta.LINE_SEP
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.js.*

/** stringifier for JavaScript */
class Stringifier(
  detail: Boolean,
  location: Boolean,
  grammar: Option[Grammar],
) {
  // elements
  given elemRule: Rule[JsElem] = (app, elem) =>
    elem match {
      case elem: Ast => astRule(app, elem)
    }

  // abstract syntax tree (AST) values
  given astRule: Rule[Ast] = (app, ast) =>
    grammar match
      case Some(grammar) => grammarAstRule(app, (grammar, ast))
      case None          => basicAstRule(app, ast)

  lazy val grammarAstRule: Rule[(Grammar, Ast)] = (app, pair) =>
    val (grammar, origAst) = pair
    val nameMap = grammar.nameMap
    def aux(ast: Ast): Unit = ast match
      case Lexical(name, str) => app >> str >> " "
      case Syntactic(name, args, rhsIdx, children) =>
        var cs = children
        for (symbol <- nameMap(name).rhsList(rhsIdx).symbols) symbol match
          case Terminal(term)                          => app >> term >> " "
          case Empty | NoLineTerminator | _: Lookahead =>
          case _ =>
            cs match
              case hd :: tl => hd.map(aux); cs = tl
              case _        => error(s"invalid AST: $origAst")
    aux(origAst)
    app

  lazy val basicAstRule: Rule[Ast] = (app, ast) =>
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
