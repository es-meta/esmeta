package esmeta.mutator

import esmeta.synthesizer.*
import esmeta.mutator.*
import esmeta.es.*
import esmeta.es.util.{Walker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.ty.AstSingleTy

/** A nearest ECMAScript AST mutator */
class WeightedMutator(using grammar: Grammar)(
  val mutators: (Mutator)*,
) extends Mutator {

  def calculateWeight(ast: Ast): Int = 0

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] =
    val weights = mutators.map(_.calculateWeight(ast))
    val pairs = mutators.zip(weights)
    weightedChoose(pairs)(ast, n, target)

  val names = mutators.toList.flatMap(_.names).sorted.distinct
}
