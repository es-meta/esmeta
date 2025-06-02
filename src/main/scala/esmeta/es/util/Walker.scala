package esmeta.es.util

import esmeta.util.BasicWalker
import esmeta.es.*

/** a walker for ECMAScript */
trait Walker extends BasicWalker {
  def walk(elem: ESElem): ESElem = elem match
    case elem: Ast => walk(elem)

  /** ASTs */
  def walk(ast: Ast): Ast = ast match
    case ast: Syntactic => walk(ast)
    case ast: Lexical   => walk(ast)
    case ast: Hole      => walk(ast)

  /** syntactic productions */
  def walk(ast: Syntactic): Syntactic =
    val Syntactic(name, args, rhsIdx, children) = ast
    Syntactic(name, args, rhsIdx, walkVector(children, walkOpt(_, walk)))

  /** lexical productions */
  def walk(ast: Lexical): Lexical = ast

  /** partial proudctions */
  def walk(ast: Hole): Hole = ast
}
