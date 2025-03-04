package esmeta.analyzer.tychecker

import esmeta.util.Appender.*

/** abstract return values */
trait AbsRetDecl { self: TyChecker =>

  case class AbsRet(value: AbsValue, effect: Effect) extends AbsRetLike {
    import AbsRet.*

    /** bottom check */
    def isBottom: Boolean = value.isBottom && effect.isBottom

    /** partial order */
    def ⊑(that: AbsRet)(using AbsState): Boolean =
      this.value ⊑ that.value && this.effect ⊑ that.effect

    /** not partial order */
    def !⊑(that: AbsRet)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsRet)(using AbsState): AbsRet =
      AbsRet(this.value ⊔ that.value, this.effect ⊔ that.effect)

    /** meet operator */
    def ⊓(that: AbsRet)(using AbsState): AbsRet =
      AbsRet(this.value ⊓ that.value, this.effect ⊓ that.effect)
  }
  object AbsRet extends DomainLike[AbsRet] {

    /** top element */
    lazy val Top: AbsRet = AbsRet(AbsValue.Top, Effect.Top)

    /** bottom element */
    lazy val Bot: AbsRet = AbsRet(AbsValue.Bot, Effect.Bot)

    /** appender */
    given effectRule: Rule[Set[String]] = iterableRule("(", ", ", ")")
    given rule: Rule[AbsRet] = (app, elem) =>
      app >> elem.value >> " " >> elem.effect
  }
}
