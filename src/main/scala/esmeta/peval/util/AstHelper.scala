package esmeta.peval.util

import esmeta.error.InvalidAstField
import esmeta.es.{Ast, Lexical, Syntactic}
import esmeta.peval.{FUNC_DECL}
import esmeta.state.{Str}

object AstHelper {
  val getChildByName = (ast: Ast, name: String) =>
    aux(ast, name).headOption.getOrElse(throw InvalidAstField(ast, Str(name)))

  val getAllChildrenByName = aux(_, _)

  val getFuncDecls = (ast: Ast) => getAllChildrenByName(ast, FUNC_DECL)

  private def aux(ast: Ast, name: String): List[Ast] = ast match
    case l @ Lexical(n, str) if (n == name) => List(l)
    case s @ Syntactic(n, _, _, children) =>
      val fromChildren = children
        .map(
          _.map(aux(_, name)).getOrElse(Nil),
        )
        .flatten
      val fromS = if (n == name) then List(s) else Nil
      fromS ::: fromChildren
    case _ => Nil
}
