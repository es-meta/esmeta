package esmeta.analyzer.tychecker

import esmeta.util.Appender.*

/** abstract return values */
trait AbsRetDecl { self: TyChecker =>

  case class AbsRet(value: AbsValue) extends AbsRetLike {
    import AbsRet.*

    /** bottom check */
    def isBottom: Boolean = value.isBottom

    /** bottom check */
    def state: AbsState = AbsState.Empty

    /** partial order */
    def ⊑(that: AbsRet)(using AbsState): Boolean = value ⊑ that.value

    /** not partial order */
    def !⊑(that: AbsRet)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsRet)(using AbsState): AbsRet = AbsRet(value ⊔ that.value)

    /** meet operator */
    def ⊓(that: AbsRet)(using AbsState): AbsRet = AbsRet(value ⊓ that.value)
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
