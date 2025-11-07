package esmeta.fuzzer

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** mutation target selector for fuzzer */
trait TargetSelector {

  /** target selection */
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView])

  /** Possible names of underlying selectors */
  val names: List[String]
}

/** weighted mutation target selector */
class WeightedSelector(pairs: (TargetSelector, Int)*) extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView]) = weightedChoose(pairs)(pool, cov)

  val names = pairs.toList.flatMap(_._1.names)
}

/** branch coverage-based mutation target selector */
object BranchSelector extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView]) =
    if (!cov.targetCondViews.isEmpty)
      val cond = choose(cov.targetCondViews.keys)
      val view = choose(cov.targetCondViews(cond))._1
      val condView = CondView(cond, view)
      cov.getScript(condView).fold(RandomSelector(pool, cov)) {
        (names.head, _, Some(condView))
      }
    else RandomSelector(pool, cov)

  val names = List("BranchTarget")
}

/** random mutation target selector */
object RandomSelector extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView]) = (names.head, choose(pool), None)

  val names = List("RandomTarget")
}
