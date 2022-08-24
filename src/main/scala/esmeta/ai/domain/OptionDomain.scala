package esmeta.ai.domain

import esmeta.ai.*
import esmeta.util.Appender.*

/** option domain */
class OptionDomain[V, D <: Domain[V] with Singleton](val AbsV: D)
  extends Domain[Option[V]] {

  /** astract V type domain */
  type AbsV = AbsV.Elem

  /** elements */
  case class Elem(value: AbsV, absent: AbsAbsent)

  /** top element */
  val Top: Elem = Elem(AbsV.Top, AbsAbsent.Top)

  /** bottom element */
  val Bot: Elem = Elem(AbsV.Bot, AbsAbsent.Bot)

  /** empty element */
  val Empty: Elem = Elem(AbsV.Bot, AbsAbsent.Top)

  /** abstraction functions */
  def alpha(xs: Iterable[Option[V]]): Elem = Elem(
    AbsV(xs.collect { case Some(x) => x }),
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
    override def gamma: BSet[Option[V]] =
      elem.value.gamma.map(Some(_)) ⊔
      (if (elem.absent.isTop) Fin(None) else Fin())

    /** get single value */
    override def getSingle: Flat[Option[V]] =
      elem.value.getSingle.map(Some(_)) ⊔
      (if (elem.absent.isTop) One(None) else Zero)

    /** fold operator */
    def fold(
      domain: Domain[_] with Singleton,
    )(default: domain.Elem)(f: AbsV => domain.Elem): domain.Elem =
      f(elem.value) ⊔ (if (elem.absent.isTop) default else domain.Bot)
  }
}
