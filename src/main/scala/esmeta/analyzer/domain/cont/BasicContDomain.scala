package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.analyzer.util.*
import esmeta.cfg.*
import esmeta.state.*
import esmeta.ir.Name
import esmeta.util.Appender.*

/** basic abstract continuations */
object BasicContDomain extends Domain {
  case object Bot extends Elem
  case class ContElem(
    target: NodePoint[Node],
    captured: Map[Name, AbsValue],
  ) extends Elem

  // constructors
  def apply(cont: ACont): Elem = ContElem(
    cont.target,
    cont.captured,
  )
  def apply(
    target: NodePoint[Node],
    captured: Map[Name, AbsValue],
  ): Elem = ContElem(target, captured)

  // appender
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot =>
        app >> "⊥"
      case ContElem(target, captured) =>
        app >> ACont(target, captured).toString

  // elements
  sealed trait Elem extends Iterable[ACont] with ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match
      case (Bot, _) => true
      case (_, Bot) => false
      case (l: ContElem, r: ContElem) => (
        l.target == r.target &&
        l.captured.keySet == r.captured.keySet &&
        l.captured.keySet.forall(x => l.captured(x) ⊑ r.captured(x))
      )

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match
      case (Bot, _) => that
      case (_, Bot) => this
      case (l: ContElem, r: ContElem)
          if (
            l.target == r.target &&
            l.captured.keySet == r.captured.keySet
          ) =>
        ContElem(
          l.target,
          l.captured.keySet
            .map(x => x -> (l.captured(x) ⊔ r.captured(x)))
            .toMap,
        )
      case _ => exploded(s"join of continuations.")

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      case (l: ContElem, r: ContElem)
          if (
            l.target == r.target &&
            l.captured.keySet == r.captured.keySet
          ) =>
        ContElem(
          l.target,
          l.captured.keySet
            .map(x => x -> (l.captured(x) ⊓ r.captured(x)))
            .toMap,
        )
      case _ => exploded(s"meet of continuations.")

    // minus operator
    def -(that: Elem): Elem = (this, that) match
      case (Bot, _)                   => Bot
      case (_, Bot)                   => this
      case (l: ContElem, r: ContElem) => ???

    // get single value
    def getSingle: Flat[ACont] = this match
      case Bot => FlatBot
      case ContElem(target, captured) =>
        FlatElem(ACont(target, captured))

    // iterators
    final def iterator: Iterator[ACont] = (this match {
      case Bot                        => None
      case ContElem(target, captured) => Some(ACont(target, captured))
    }).iterator
  }
}
