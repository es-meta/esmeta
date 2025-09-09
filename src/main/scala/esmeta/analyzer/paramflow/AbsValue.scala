package esmeta.analyzer.paramflow

import esmeta.util.Appender.*

/** abstract values */
trait AbsValueDecl { self: ParamFlowAnalyzer =>

  case class AbsValue(
    params: Set[ParamKind] = Set(),
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
    def apply(xs: ParamKind*): AbsValue = AbsValue(params = xs.toSet)

    /** appender */
    given rule: Rule[AbsValue] = (app, value) => {
      given Rule[List[ParamKind]] = iterableRule("[", ", ", "]")
      val AbsValue(params) = value
      app >> params.toList.sorted
    }

    given paramKindRule: Rule[ParamKind] = (app, pk) =>
      import ParamKind.*
      pk match
        case This        => app >> "this"
        case ThisIdx(k)  => app >> s"this[$k]"
        case Named(name) => app >> name

    given paramKindOrder: Ordering[ParamKind] =
      import ParamKind.*
      Ordering.by {
        case This        => (0, "")
        case ThisIdx(k)  => (1, k.toString)
        case Named(name) => (2, name)
      }
  }
}

enum ParamKind:
  case This
  case ThisIdx(k: Int)
  case Named(name: String)
