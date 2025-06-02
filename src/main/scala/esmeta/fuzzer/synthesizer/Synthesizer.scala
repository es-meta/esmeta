package esmeta.fuzzer.synthesizer

import esmeta.cfg.*
import esmeta.es.util.*
import esmeta.es.*
import esmeta.spec.Grammar

/** ECMAScript AST synthesizer */
trait Synthesizer {

  /** synthesizer name */
  def name: String

  /** for general production */
  def apply(ast: Ast): Ast = ast match
    case ast: Syntactic => apply(ast)
    case ast: Lexical   => apply(ast)
    case ast: Hole      => apply(ast)

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic
  def apply(ast: Syntactic): Syntactic = apply(ast.name, ast.args)

  /** for lexical production */
  def apply(name: String): Lexical
  def apply(ast: Lexical): Lexical = apply(ast.name)

  def apply(ast: Hole): Hole = ???
}
object Synthesizer:
  type Builder = Grammar => Synthesizer
