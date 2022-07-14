package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.ir.Name
import esmeta.util.Appender
import esmeta.util.Appender.*

/** basic abstract continuations */
object BasicContDomain extends Domain {
  case object Bot extends Elem
  case class ContElem(
    captured: Map[Name, AbsValue],
    target: NodePoint[Node],
  ) extends Elem

  // constructors
  def apply(cont: ACont): Elem = ContElem(
    cont.captured,
    cont.target,
  )
  def apply(
    captured: Map[Name, AbsValue],
    target: NodePoint[Node],
  ): Elem = ContElem(captured, target)

  // appender
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot =>
        app >> "⊥"
      case ContElem(captured, target) =>
        app >> ACont(captured, target).toString

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
          l.captured.keySet
            .map(x => x -> (l.captured(x) ⊔ r.captured(x)))
            .toMap,
          l.target,
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
          l.captured.keySet
            .map(x => x -> (l.captured(x) ⊓ r.captured(x)))
            .toMap,
          l.target,
        )
      case _ => exploded(s"meet of continuations.")

    // get single value
    def getSingle: Flat[ACont] = this match
      case Bot => FlatBot
      case ContElem(captured, target) =>
        FlatElem(ACont(captured, target))

    // iterators
    final def iterator: Iterator[ACont] = (this match {
      case Bot                        => None
      case ContElem(captured, target) => Some(ACont(captured, target))
    }).iterator
  }
}
