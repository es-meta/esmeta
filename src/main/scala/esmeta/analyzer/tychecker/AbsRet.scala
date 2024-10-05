package esmeta.analyzer.tychecker

import esmeta.util.Appender.*

/** abstract return values */
trait AbsRetDecl { self: TyChecker =>

  case class AbsRet(
    value: AbsValue,
    state: AbsState,
  ) extends AbsRetLike {
    import AbsRet.*

    /** bottom check */
    def isBottom: Boolean = value.isBottom

    /** partial order */
    def ⊑(that: AbsRet)(using AbsState): Boolean =
      this.value ⊑ that.value && this.state ⊑ that.state

    /** not partial order */
    def !⊑(that: AbsRet)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsRet)(using AbsState): AbsRet = AbsRet(
      this.value ⊔ that.value,
      this.state ⊔ that.state,
    )

    /** meet operator */
    def ⊓(that: AbsRet)(using AbsState): AbsRet = AbsRet(
      value ⊓ that.value,
      state ⊓ that.state,
    )
  }
  object AbsRet extends DomainLike[AbsRet] {

    /** top element */
    lazy val Top: AbsRet = AbsRet(AbsValue.Top, AbsState.Top)

    /** bottom element */
    lazy val Bot: AbsRet = AbsRet(AbsValue.Bot, AbsState.Bot)

    /** appender */
    given rule: Rule[AbsRet] = (app, elem) =>
      val AbsRet(value, state) = elem
      app >> value
      if (!state.isBottom) app >> " " >> state
      else app
  }
}
