package esmeta.analyzer.astflow

import esmeta.cfg.*
import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.{*, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** abstract values */
trait AbsValueDecl { self: AstFlowAnalyzer =>

  case class AbsValue(
    params: Set[Param] = Set(),
    asts: Set[AstExpr] = Set(),
  ) extends AbsValueLike {

    /** bottom check */
    def isBottom: Boolean = params.isEmpty && asts.isEmpty

    /** partial order */
    def ⊑(that: AbsValue)(using st: AbsState): Boolean =
      (this.params subsetOf that.params) &&
      (this.asts subsetOf that.asts)

    /** not partial order */
    def !⊑(that: AbsValue)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsValue)(using st: AbsState): AbsValue = AbsValue(
      params = this.params ++ that.params,
      asts = this.asts ++ that.asts,
    )

    /** meet operator */
    def ⊓(that: AbsValue)(using st: AbsState): AbsValue =
      AbsValue(
        params = this.params intersect that.params,
        asts = this.asts intersect that.asts,
      )

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String = this.toString
  }
  object AbsValue extends DomainLike[AbsValue] {

    /** top element */
    lazy val Top: AbsValue = exploded("top abstract state")

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue()

    /** create abstract value from parameters */
    def param(a: Param*): AbsValue = AbsValue(params = a.toSet)

    /** create abstract value from asts */
    def ast(a: AstExpr*): AbsValue = AbsValue(asts = a.toSet)

    /** appender */
    given rule: Rule[AbsValue] = (app, value) => {
      given Rule[List[String]] = iterableRule("[", ", ", "]")
      val AbsValue(params, asts) = value
      val sources = params.map("\"" + _.lhs + "\"") // ++ asts.map(x => x)
      app >> sources.toList.map(_.toString).sorted
    }
  }
}
