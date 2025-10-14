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

  val weight: Int = 0

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
    elapsedBlock: Int,
  ): Seq[Result] = chooseMutator(code, n, target, elapsedBlock)

  /** mutate ASTs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Ast] = chooseMutator(ast, n, target)

  /** choose a mutator under weight */
  def chooseMutator: Mutator =
    val weights = mutators.map(_.weight)
    weightedChoose(mutators zip weights)

  val names = mutators.toList.flatMap(_.names).sorted.distinct
}
object WeightedMutator {
  def apply(mutators: Mutator*)(using CFG): WeightedMutator =
    new WeightedMutator(mutators)
}
