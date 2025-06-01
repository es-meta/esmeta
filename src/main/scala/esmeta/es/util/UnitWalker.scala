package esmeta.es.util

import esmeta.util.BasicUnitWalker
import esmeta.es.*

/** a unit walker for ECMAScript */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: ESElem): Unit = elem match
    case elem: Ast => walk(elem)

  /** ASTs */
  def walk(ast: Ast): Unit = ast match
    case ast: Syntactic => walk(ast)
    case ast: Lexical   => walk(ast)
    case ast: Hole      => walk(ast)

  /** syntactic productions */
  def walk(ast: Syntactic): Unit =
    walkVector(ast.children, walkOpt(_, walk))

  /** lexical productions */
  def walk(ast: Lexical): Unit = {}

  /** partial productions */
  def walk(ast: Hole): Unit = {}
}
