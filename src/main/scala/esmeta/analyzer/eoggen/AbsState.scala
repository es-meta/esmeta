package esmeta.analyzer.eoggen

import esmeta.ir.*
import esmeta.ty.{*, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** abstract states */
trait AbsStateDecl { self: EOGGenerator =>
  import irStringifier.given

  case class AbsState(
    locals: Map[Local, AbsValue],
  ) extends AbsStateLike {
    import AbsState.*

    given AbsState = this

    /** bottom check */
    def isBottom: Boolean = locals.isEmpty

    /** partial order */
    def ⊑(that: AbsState): Boolean = (this, that) match
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (l, r)             => l.locals.forall { (x, v) => v ⊑ r(x) }

    /** not partial order */
    def !⊑(that: AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (l, r) =>
        AbsState((for {
          x <- l.locals.keySet ++ r.locals.keySet
        } yield x -> l(x) ⊔ r(x)).toMap)

    /** meet operator */
    def ⊓(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (l, r) =>
        AbsState((for {
          x <- l.locals.keySet intersect r.locals.keySet
        } yield x -> l(x) ⊓ r(x)).toMap)

    /** get the absvalue value of a local variable */
    def apply(x: Local): AbsValue = locals.getOrElse(x, AbsValue.Bot)

    /** has imprecise elements */
    def hasImprec: Boolean = false

    /** define variables */
    def define(x: Local, v: AbsValue): AbsState = AbsState(locals + (x -> v))
  }
  object AbsState extends DomainLike[AbsState] {

    /** top element */
    lazy val Top: AbsState = exploded("top abstract state")

    /** bottom element */
    lazy val Bot: AbsState = AbsState(Map())

    /** constructor */
    def apply(pairs: (Local, AbsValue)*): AbsState = AbsState(pairs.toMap)

    /** appender */
    given rule: Rule[AbsState] = (app, st) =>
      import esmeta.ir.given
      given localsRule: Rule[Map[Local, AbsValue]] = sortedMapRule(sep = ": ")
      app >> st.locals
  }
}
