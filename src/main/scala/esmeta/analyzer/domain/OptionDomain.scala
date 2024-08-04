package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.util.*
import esmeta.util.Appender.*

trait OptionDomainDecl { self: Self =>

  /** option domain */
  class OptionDomain[V, D <: Domain[V] with Singleton](val AbsV: D)
    extends Domain[Option[V]] {

    /** astract V type domain */
    type AbsV = AbsV.Elem

    /** elements */
    case class Elem(value: AbsV, uninit: AbsUnint) extends Appendable

    /** top element */
    lazy val Top: Elem = Elem(AbsV.Top, AbsUnint.Top)

    /** bottom element */
    lazy val Bot: Elem = Elem(AbsV.Bot, AbsUnint.Bot)

    /** empty element */
    lazy val Empty: Elem = Elem(AbsV.Bot, AbsUnint.Top)

    /** abstraction functions */
    def alpha(xs: Iterable[Option[V]]): Elem = Elem(
      AbsV(xs.collect { case Some(x) => x }),
      if (xs.exists(_ == None)) AbsUnint.Top else AbsUnint.Bot,
    )

    /** constructor */
    def apply(
      value: AbsV = AbsV.Bot,
      uninit: AbsUnint = AbsUnint.Bot,
    ): Elem = Elem(value, uninit)

    /** appender */
    given rule: Rule[Elem] = (app, elem) =>
      app >> elem.value >> (if (elem.uninit.isTop) "?" else "")

    /** element interfaces */
    extension (elem: Elem) {

      /** partial order */
      def ⊑(that: Elem): Boolean =
        elem.value ⊑ that.value &&
        elem.uninit ⊑ that.uninit

      /** join operator */
      def ⊔(that: Elem): Elem = Elem(
        elem.value ⊔ that.value,
        elem.uninit ⊔ that.uninit,
      )

      /** meet operator */
      override def ⊓(that: Elem): Elem = Elem(
        elem.value ⊓ that.value,
        elem.uninit ⊓ that.uninit,
      )

      /** prune operator */
      override def --(that: Elem): Elem = Elem(
        elem.value -- that.value,
        elem.uninit -- that.uninit,
      )

      /** concretization function */
      override def gamma: BSet[Option[V]] =
        elem.value.gamma.map(Some(_)) ⊔
        (if (elem.uninit.isTop) Fin(None) else Fin())

      /** get single value */
      override def getSingle: Flat[Option[V]] =
        elem.value.getSingle.map(Some(_)) ⊔
        (if (elem.uninit.isTop) One(None) else Zero)

      /** single check */
      def isSingle: Boolean = elem.getSingle match
        case One(_) => true
        case _      => false

      /** fold operator */
      def fold(
        domain: Domain[_] with Singleton,
      )(default: domain.Elem)(f: AbsV => domain.Elem): domain.Elem =
        f(elem.value) ⊔ (if (elem.uninit.isTop) default else domain.Bot)
    }
  }
}
