package esmeta.fuzzer.mutator

import esmeta.fuzzer.synthesizer.*
import esmeta.es.*
import esmeta.es.util.{Walker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG

/** A mutator selects one of given mutators under weight */
class WeightedMutator(using cfg: CFG)(
  val mutators: Seq[Mutator],
) extends Mutator {
  import Mutator.*

  def calculateWeight(ast: Ast): Int = 0

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] =
    val weights = mutators.map(_.calculateWeight(ast))
    weightedChoose(mutators zip weights)(ast, n, target)

  val names = mutators.toList.flatMap(_.names).sorted.distinct
}
object WeightedMutator {
  def apply(mutators: Mutator*)(using CFG): WeightedMutator =
    new WeightedMutator(mutators)
}
