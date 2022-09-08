package esmeta.es.util.mutator

import esmeta.es.*

trait Mutator {
  val name: String
  def mutate(ast: Ast): Ast
}
