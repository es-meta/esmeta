package esmeta.analyzer.tychecker

import esmeta.util.Appender.*

/** abstract return values */
trait AbsRetDecl { self: TyChecker =>

  case class AbsRet(
    value: AbsValue = AbsValue.Bot,
    noSym: (AbsValue, TypeProp) = (AbsValue.Bot, TypeProp.Bot),
    syms: Map[NodePoint[?], (AbsValue, TypeProp)] = Map.empty,
    effect: Effect = Effect.Bot,
  ) extends AbsRetLike {
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
      val (lv, lc) = this.noSym
      val (rv, rc) = that.noSym
      val syms: Map[NodePoint[?], (AbsValue, TypeProp)] = (for {
        np <- (this.syms.keySet ++ that.syms.keySet).toList
        pair = (this.syms.get(np), that.syms.get(np)) match
          case (Some((v1, c1)), Some((v2, c2))) => (v1 ⊔ v2, c1 || c2)
          case (Some(p), None)                  => p
          case (None, Some(p))                  => p
          case (None, None)                     => (AbsValue.Bot, TypeProp.Bot)
      } yield np -> pair).toMap
      AbsRet(
        this.value ⊔ that.value,
        (lv ⊔ rv, lc || rc),
        syms,
        this.effect ⊔ that.effect,
      )

    /** meet operator */
    def ⊓(that: AbsRet)(using AbsState): AbsRet =
      val (lv, lc) = this.noSym
      val (rv, rc) = that.noSym
      val syms: Map[NodePoint[?], (AbsValue, TypeProp)] = (for {
        np <- (this.syms.keySet intersect that.syms.keySet).toList
        (v1, c1) = this.syms(np)
        (v2, c2) = that.syms(np)
      } yield np -> (v1 ⊓ v2, c1 && c2)).toMap
      AbsRet(
        this.value ⊓ that.value,
        (lv ⊓ rv, lc && rc),
        syms,
        this.effect ⊓ that.effect,
      )
  }
  object AbsRet extends DomainLike[AbsRet] {

    /** top element */
    lazy val Top: AbsRet = AbsRet(AbsValue.Top)

    /** bottom element */
    lazy val Bot: AbsRet = AbsRet(AbsValue.Bot)

    /** appender */
    given rule: Rule[AbsRet] = (app, elem) =>
      val AbsRet(value, (v, m), syms, effect) = elem
      app.wrap {
        app :> "- value: " >> value
        if (effect.nonEmpty) app :> "- effect: " >> effect
        app :> "- noSym: " >> v >> " (" >> m >> ")"
        app :> "- syms(" >> syms.size >> "): "
        app.wrap {
          for ((np, (v, constr)) <- elem.syms.toList.sortBy(_._1.node.id))
            app :> np.node.name >> " -> " >> v >> " (" >> constr >> ")"
        }
      }
  }
}
