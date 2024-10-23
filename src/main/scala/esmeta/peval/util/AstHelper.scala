package esmeta.peval.util

import esmeta.error.{InvalidAstField, PartialEvaluatorError}
import esmeta.es.{Ast, Lexical, Syntactic}
import esmeta.ir.{Expr}
import esmeta.peval.{FUNC_DECL}
import esmeta.state.{Str}
import scala.collection.immutable.Range.Partial
import esmeta.peval.PartialEvaluator

object AstHelper {
  val getChildByName = (ast: Ast, name: String) =>
    getAllChildrenByName(ast, name).headOption.getOrElse(
      throw InvalidAstField(ast, Str(name)),
    )

  val getFuncDecls = (ast: Ast) => getAllChildrenByName(ast, FUNC_DECL)

  def getAllChildrenByName(ast: Ast, name: String): List[Ast] =
    val fromChildren = for {
      childOpt <- ast.children
      child <- childOpt
      result = getAllChildrenByName(child, name)
    } yield result
    val fromThis = if (ast.name == name) then List(ast) else Nil
    fromThis ::: fromChildren.flatten

  /** auxilaray function for partial evaluators Sdo call
    *
    * @param ast
    *   a root ast to be explored
    * @param sub
    *   a sub ast to be found
    *
    * @return
    *   an expression representing the sub ast.
    * @throws InvalidAstField
    *   if the sub ast is not found.
    */
  def getSubgraphPath(root: Ast, sub: Ast): List[Int] =
    /* aux for getSubgraphPath to do recursion */
    def aux(now: Ast): Option[List[Int]] = now == sub match
      case true => Some(Nil)
      case false =>
        lazy val result = (for {
          (childOpt, idx) <- now.children.zipWithIndex
          child <- childOpt
          sublist <- aux(child)
          result = idx :: sublist
        } yield result).headOption
        result
    aux(root) match
      case Some(value) => value
      case None =>
        throw PartialEvaluatorError(s"getRepresent: $sub not found in $root")

}
