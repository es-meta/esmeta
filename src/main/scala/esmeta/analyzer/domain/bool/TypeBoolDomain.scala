package esmeta.analyzer.domain

import esmeta.state.Bool
import esmeta.util.Appender
import esmeta.util.Appender.*

/** type domain for boolean */
object TypeBoolDomain extends BoolDomain {
  // elements
  case object Bot extends Elem
  case object Top extends Elem

  // appender
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot => app >> "⊥"
      case Top => app >> "BoolT"

  // abstraction
  def apply(elems: Bool*): Elem = elems.size match
    case 0 => Bot
    case _ => Top

  // interfaces
  extension (elem: Elem) {
    def unary_! : Elem = elem
    def ||(that: Elem): Elem = elem ⊓ that
    def &&(that: Elem): Elem = elem ⊓ that
    def ⊓(that: Elem): Elem = (elem, that) match
      case (Bot, _) | (_, Bot) => Bot
      case _                   => Top
    def -(that: Elem): Elem = that match
      case Bot => elem
      case Top => Bot
    def getSingle: Flat[Bool] = elem match
      case Bot => FlatBot
      case Top => FlatTop
  }

  // elements
  trait Elem extends ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match
      case (Bot, _) | (_, Top) => true
      case (Top, Bot)          => false

    // join
    def ⊔(that: Elem): Elem = (this, that) match
      case (Bot, Bot) => Bot
      case _          => Top

  }
}
