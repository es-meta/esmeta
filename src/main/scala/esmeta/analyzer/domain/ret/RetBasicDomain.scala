package esmeta.analyzer.domain.ret

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.Appender
import esmeta.util.Appender.*

trait RetBasicDomainDecl { self: Self =>

  /** basic domain for return values */
  object RetBasicDomain extends RetDomain {

    /** elements */
    case class Elem(
      value: AbsValue = AbsValue.Bot,
      state: AbsState = AbsState.Bot,
    ) extends Appendable

    /** top element */
    lazy val Top = Elem(AbsValue.Top, AbsState.Top)

    /** bottom element */
    lazy val Bot = Elem()

    /** abstraction functions */
    def alpha(xs: Iterable[(AValue, State)]): Elem = ???
    // val (vs, ss) = xs.unzip
    // Elem(AbsValue(vs), AbsState(ss))

    /** constructors */
    def apply(
      value: AbsValue = AbsValue.Bot,
      state: AbsState = AbsState.Bot,
    ): Elem = Elem(value, state)

    /** extractors */
    def unapply(elem: Elem): (AbsValue, AbsState) =
      val Elem(value, state) = elem
      (value, state)

    /** appender */
    given rule: Rule[Elem] = (app, elem) => app >> elem.value

    /** element interfaces */
    extension (elem: Elem) {

      /** partial order */
      def ⊑(that: Elem): Boolean =
        elem.value ⊑ that.value &&
        elem.state ⊑ that.state

      /** join operator */
      def ⊔(that: Elem): Elem = Elem(
        elem.value ⊔ that.value,
        elem.state ⊔ that.state,
      )

      /** meet operator */
      override def ⊓(that: Elem): Elem = Elem(
        elem.value ⊓ that.value,
        elem.state ⊓ that.state,
      )

      /** minus operator */
      override def --(that: Elem): Elem = Elem(
        elem.value -- that.value,
        elem.state -- that.state,
      )

      /** wrap completion records */
      def wrapCompletion: Elem = Elem(elem.value.wrapCompletion, elem.state)

      /** getters */
      def value: AbsValue = elem.value
      def state: AbsState = elem.state

      /** conversion to string */
      def getString(detail: Boolean): String =
        val app = Appender()
        if (detail) {
          app >> elem >> LINE_SEP
          app >> "globals: "
          app.wrap {
            for ((k, v) <- state.globals.toList.sortBy(_._1.toString)) {
              app :> s"$k -> $v" >> LINE_SEP
            }
          } >> LINE_SEP
          app >> "heap: " >> state.heap
        } else app >> elem
        app.toString
    }
  }
}
