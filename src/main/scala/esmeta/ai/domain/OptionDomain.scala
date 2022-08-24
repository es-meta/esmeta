package esmeta.ai.domain

import esmeta.ai.*
import esmeta.util.Appender.*

/** option domain */
class OptionDomain[A, D <: Domain[A]](
  val config: Config,
  val AbsA: D,
) extends Domain[Option[A]] {
  import config._
  type AbsA = AbsA.Elem

  /** elements */
  case class Elem(value: AbsA, absent: AbsAbsent)

  /** top element */
  val Top: Elem = Elem(AbsA.Top, AbsAbsent.Top)

  /** bottom element */
  val Bot: Elem = Elem(AbsA.Bot, AbsAbsent.Bot)

  /** empty element */
  val Empty: Elem = Elem(AbsA.Bot, AbsAbsent.Top)

  /** abstraction functions */
  def alpha(xs: Iterable[Option[A]]): Elem = Elem(
    AbsA(xs.collect { case Some(x) => x }),
    if (xs.exists(_ == None)) AbsAbsent.Top else AbsAbsent.Bot,
  )

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    app >> elem.value >> (if (elem.absent.isTop) "?" else "")

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean =
      elem.value ⊑ that.value &&
      elem.absent ⊑ that.absent

    /** join operator */
    def ⊔(that: Elem): Elem = Elem(
      elem.value ⊔ that.value,
      elem.absent ⊔ that.absent,
    )

    /** meet operator */
    override def ⊓(that: Elem): Elem = Elem(
      elem.value ⊓ that.value,
      elem.absent ⊓ that.absent,
    )

    /** prune operator */
    override def -(that: Elem): Elem = Elem(
      elem.value - that.value,
      elem.absent - that.absent,
    )

    /** concretization function */
    override def gamma: BSet[Option[A]] =
      elem.value.gamma.map(Some(_)) ⊔
      (if (elem.absent.isTop) Fin(None) else Fin())

    /** get single value */
    override def getSingle: Flat[Option[A]] =
      elem.value.getSingle.map(Some(_)) ⊔
      (if (elem.absent.isTop) One(None) else Zero)
  }
}
