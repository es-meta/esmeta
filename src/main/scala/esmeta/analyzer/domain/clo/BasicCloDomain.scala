package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.ir.Name
import esmeta.util.Appender
import esmeta.util.Appender.*

/** basic abstract closures */
object BasicCloDomain extends Domain {
  case object Bot extends Elem
  case class CloElem(
    captured: Map[Name, AbsValue],
    func: Func,
  ) extends Elem

  // constructors
  def apply(clo: AClo): Elem = CloElem(
    clo.captured,
    clo.func,
  )
  def apply(
    captured: Map[Name, AbsValue],
    func: Func,
  ): Elem = CloElem(captured, func)

  // appender
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot =>
        app >> "⊥"
      case CloElem(captured, func) =>
        app >> AClo(captured, func).toString

  // elements
  sealed trait Elem extends Iterable[AClo] with ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match
      case (Bot, _) => true
      case (_, Bot) => false
      case (l: CloElem, r: CloElem) => (
        l.func == r.func &&
        l.captured.keySet == r.captured.keySet &&
        l.captured.keySet.forall(x => l.captured(x) ⊑ r.captured(x))
      )

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match
      case (Bot, _) => that
      case (_, Bot) => this
      case (l: CloElem, r: CloElem)
          if (
            l.func == r.func &&
            l.captured.keySet == r.captured.keySet
          ) =>
        CloElem(
          l.captured.keySet
            .map(x => x -> (l.captured(x) ⊔ r.captured(x)))
            .toMap,
          l.func,
        )
      case _ => exploded(s"join of closures.")

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      case (l: CloElem, r: CloElem)
          if (
            l.func == r.func &&
            l.captured.keySet == r.captured.keySet
          ) =>
        CloElem(
          l.captured.keySet
            .map(x => x -> (l.captured(x) ⊓ r.captured(x)))
            .toMap,
          l.func,
        )
      case _ => exploded(s"meet of closures.")

    // get single value
    def getSingle: Flat[AClo] = this match
      case Bot => FlatBot
      case CloElem(captured, func) =>
        FlatElem(AClo(captured, func))

    // iterators
    final def iterator: Iterator[AClo] = (this match {
      case Bot                     => None
      case CloElem(captured, func) => Some(AClo(captured, func))
    }).iterator
  }
}
