package esmeta.analyzer.domain

import esmeta.util.Appender.*

/** simple domain */
trait SimpleDomain[A] extends Domain {
  // target value
  protected val value: A

  // elements
  object Bot extends Elem
  object Top extends Elem

  // appender
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot => app >> "⊥"
      case Top => app >> value.toString

  // elements
  sealed trait Elem extends ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match
      case (Bot, _) | (_, Top) => true
      case (_, Bot) | (Top, _) => false

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match
      case (Bot, _) | (_, Top) => that
      case (_, Bot) | (Top, _) => this

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match
      case (Bot, _) | (_, Top) => this
      case (_, Bot) | (Top, _) => that

    // minus operator
    def -(that: Elem): Elem = that match
      case Bot => this
      case Top => Bot

    // get single value
    def getSingle: Flat[value.type] = this match
      case Bot => FlatBot
      case Top => FlatElem(value)
  }
}
object SimpleDomain {
  // constructors
  def apply[A](v: A): SimpleDomain[A] =
    new SimpleDomain[A] { protected val value: A = v }
}
