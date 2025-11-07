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
class WeightedMutator(using cfg: CFG)(pairs: (Mutator, Int)*) extends Mutator {
  import Mutator.*

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = weightedChoose(pairs)(code, n, target)

  /** mutate ASTs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Ast] = weightedChoose(pairs)(ast, n, target)

  val names = pairs.toList.flatMap(_._1.names).sorted.distinct
}
