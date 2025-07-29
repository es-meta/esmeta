package esmeta.analyzer.propflow

import esmeta.ir.*
import esmeta.ty.{*, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** abstract states */
trait AbsStateDecl { self: PropFlowAnalyzer =>
  import irStringifier.given

  case class AbsState(
    locals: Map[Local, AbsValue] = Map(),
  ) extends AbsStateLike {
    import AbsState.*

    given AbsState = this

    /** bottom check */
    def isBottom: Boolean = locals.isEmpty

    /** has imprecise elements */
    def hasImprec: Boolean = false

    /** partial order */
    def ⊑(that: AbsState): Boolean = (this, that) match
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (l, r)             => l.locals.forall { (x, lv) => lv ⊑ r(x) }

    /** not partial order */
    def !⊑(that: AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (l, r) =>
        AbsState((for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = l(x) ⊔ r(x)
        } yield x -> v).toMap)

    /** meet operator */
    def ⊓(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (l, r) =>
        AbsState((for {
          x <- (l.locals.keySet intersect r.locals.keySet).toList
          v = l(x) ⊓ r(x)
        } yield x -> v).toMap)

    /** getter */
    def apply(x: Local): AbsValue = locals.getOrElse(x, AbsValue.Bot)

    /** local identifier updater */
    def update(x: Local, value: AbsValue): AbsState =
      AbsState(locals + (x -> value))
  }
  object AbsState extends DomainLike[AbsState] {

    /** top element */
    lazy val Top: AbsState = exploded("top abstract state")

    /** bottom element */
    lazy val Bot: AbsState = AbsState()

    /** appender */
    given rule: Rule[AbsState] = (app, st) => {
      given Ordering[Local] = Ordering.by(_.toString)
      given Rule[Map[Local, AbsValue]] = sortedMapRule(sep = ": ")
      app >> st.locals
    }
  }
}
