package esmeta.analyzer.eoggen

import esmeta.util.Appender.*

/** abstract return values */
trait AbsRetDecl { self: EOGGenerator =>

  case class AbsRet(value: AbsValue) extends AbsRetLike {
    import AbsRet.*

    /** bottom check */
    def isBottom: Boolean = value.isBottom

    /** partial order */
    def ⊑(that: AbsRet)(using AbsState): Boolean = this.value ⊑ that.value

    /** not partial order */
    def !⊑(that: AbsRet)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsRet)(using AbsState): AbsRet =
      AbsRet(this.value ⊔ that.value)

    /** meet operator */
    def ⊓(that: AbsRet)(using AbsState): AbsRet =
      AbsRet(this.value ⊓ that.value)
  }
  object AbsRet extends DomainLike[AbsRet] {

    /** top element */
    lazy val Top: AbsRet = AbsRet(AbsValue.Top)

    /** bottom element */
    lazy val Bot: AbsRet = AbsRet(AbsValue.Bot)

    /** appender */
    given rule: Rule[AbsRet] = (app, elem) => app >> elem.value
  }
}
