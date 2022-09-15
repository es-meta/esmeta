package esmeta.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.spec.Grammar

/** ECMAScript AST mutator */
trait Mutator extends Walker {
  def apply(ast: Ast): Ast = walk(ast)

  /** ECMAScript grammar */
  def grammar: Grammar
}
object Mutator:
  type Builder = Grammar => Mutator
