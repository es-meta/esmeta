package esmeta.analyzer.paramflow

import esmeta.util.Appender.*

/** abstract values */
trait AbsValueDecl { self: ParamFlowAnalyzer =>

  case class AbsValue(
    params: Set[String] = Set(),
  ) extends AbsValueLike {

    /** bottom check */
    def isBottom: Boolean = params.isEmpty

    /** partial order */
    def ⊑(that: AbsValue)(using st: AbsState): Boolean =
      this.params subsetOf that.params

    /** not partial order */
    def !⊑(that: AbsValue)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      params = this.params ++ that.params,
    )

    /** meet operator */
    def ⊓(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      params = this.params intersect that.params,
    )

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String = this.toString
  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element */
    lazy val Top: AbsValue = exploded("top abstract value")

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue()

    /** create abstract value from parameters */
    def apply(xs: String*): AbsValue = AbsValue(params = xs.toSet)

    /** appender */
    given rule: Rule[AbsValue] = (app, value) => {
      given Rule[List[String]] = iterableRule("[", ", ", "]")
      val AbsValue(params) = value
      app >> params.toList.sorted
    }
  }
}
