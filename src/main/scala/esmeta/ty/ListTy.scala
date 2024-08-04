package esmeta.ty

import esmeta.state.{ListObj, Heap}
import esmeta.ty.util.Parser
import esmeta.util.*

/** list types */
enum ListTy extends TyElem with Lattice[ListTy] {
  case Top
  case Elem(value: ValueTy)
  case Bot

  import ListTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => ListTy): Boolean = (this eq that) || {
    (this, that) match
      case (Bot, _) | (_, Top) => true
      case (Top, _) | (_, Bot) => false
      case (Elem(l), Elem(r))  => l <= r
  }

  /** union type */
  def ||(that: => ListTy): ListTy =
    if (this eq that) this
    else
      (this, that) match
        case (Bot, _) | (_, Top) => that
        case (Top, _) | (_, Bot) => this
        case (Elem(l), Elem(r))  => Elem(l || r).normalized

  /** intersection type */
  def &&(that: => ListTy): ListTy =
    if (this eq that) this
    else
      (this, that) match
        case (Bot, _) | (_, Top) => that
        case (Top, _) | (_, Bot) => this
        case (Elem(l), Elem(r))  => Elem(l && r).normalized

  /** prune type */
  def --(that: => ListTy): ListTy = if (this <= that) Bot else this

  /** list element type */
  def elem: ValueTy = this match
    case Top      => AnyT
    case Elem(ty) => ty
    case Bot      => BotT

  /** list containment check */
  def contains(l: ListObj, heap: Heap): Boolean = this match
    case Top      => true
    case Bot      => false
    case Elem(ty) => l.values.forall(ty.contains(_, heap))

  /** normalized type */
  def normalized: ListTy = this match
    case Elem(elem) if elem.isTop => Top
    case _                        => this
}
object ListTy extends Parser.From(Parser.listTy) {
  def apply(elem: ValueTy): ListTy = Elem(elem).normalized
  lazy val Nil: ListTy = ListTy.Elem(ValueTy.Bot)
}
