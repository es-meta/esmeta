package esmeta.analyzer.domain.ret

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.Appender
import esmeta.util.Appender.*

trait RetTypeDomainDecl { self: Self =>

  /** type domain for return values */
  object RetTypeDomain extends RetDomain {

    /** elements */
    case class Elem(value: AbsValue = AbsValue.Bot) extends Appendable

    /** top element */
    lazy val Top = Elem(AbsValue.Top)

    /** bottom element */
    lazy val Bot = Elem()

    /** abstraction functions */
    def alpha(xs: Iterable[(AValue, State)]): Elem = ???

    /** constructors */
    def apply(
      value: AbsValue = AbsValue.Bot,
      state: AbsState = AbsState.Bot,
    ): Elem = Elem(value)

    /** extractors */
    def unapply(elem: Elem): (AbsValue, AbsState) = (elem.value, AbsState.Bot)

    /** appender */
    given rule: Rule[Elem] = (app, elem) => app >> elem.value

    /** element interfaces */
    extension (elem: Elem) {

      /** partial order */
      def ⊑(that: Elem): Boolean = elem.value ⊑ that.value

      /** join operator */
      def ⊔(that: Elem): Elem = Elem(elem.value ⊔ that.value)

      /** meet operator */
      override def ⊓(that: Elem): Elem = Elem(elem.value ⊓ that.value)

      /** minus operator */
      override def --(that: Elem): Elem = Elem(elem.value -- that.value)

      /** wrap completion records */
      def wrapCompletion: Elem = Elem(elem.value.wrapCompletion)

      /** getters */
      def value: AbsValue = elem.value
      def state: AbsState = AbsState.Bot

      /** conversion to string */
      def getString(detail: Boolean): String = elem.toString
    }
  }
}
