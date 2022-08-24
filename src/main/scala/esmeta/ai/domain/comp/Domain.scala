package esmeta.ai.domain.comp

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract completion record domain */
trait Domain extends domain.Domain[AComp] {

  /** abstraction functions for an original completion value */
  def alpha(comp: Comp): Elem = alpha(AComp.from(comp))

  /** abstraction functions */
  def alpha(ty: AbsValue, value: AbsValue, target: AbsValue): Elem
  def apply(ty: AbsValue, value: AbsValue, target: AbsValue): Elem =
    alpha(ty, value, target)

  /** constructors with maps */
  def apply(map: Map[String, Result]): Elem

  /** extractors */
  def unapply(elem: Elem): Option[Map[String, Result]]

  /** results in completion records */
  case class Result(value: AbsPureValue, target: AbsPureValue) {
    def isBottom = value.isBottom && target.isBottom
    def ⊑(that: Result): Boolean =
      this.value ⊑ that.value && this.target ⊑ that.target
    def ⊔(that: Result): Result =
      Result(this.value ⊔ that.value, this.target ⊔ that.target)
    def ⊓(that: Result): Result =
      Result(this.value ⊓ that.value, this.target ⊓ that.target)
    def -(that: Result): Result =
      Result(this.value - that.value, this.target - that.target)
  }
  object Result { val Bot = Result(AbsPureValue.Bot, AbsPureValue.Bot) }

  /** completion record element interfaces */
  extension (elem: Elem) {

    /** normal completions */
    def normal: Result

    /** remove normal completions */
    def removeNormal: Elem

    /** result of each completion type */
    def get(ty: String): Result

    /** merged result */
    def mergedResult: Result

    /** lookup */
    def apply(str: AbsStr): AbsPureValue

    /** get reachable address partitions */
    def reachableParts: Set[Part]
  }
}
