package esmeta.ty

import esmeta.state.{Value, MapObj, Heap}
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.util.domain.{*, given}, BSet.*, Flat.*

/** map types */
enum MapTy extends TyElem {
  case Top
  case Elem(key: ValueTy, value: ValueTy)
  case Bot

  import MapTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: MapTy): Boolean = (this eq that) || {
    (this, that) match
      case (Bot, _) | (_, Top)          => true
      case (Top, _) | (_, Bot)          => false
      case (Elem(lk, lv), Elem(rk, rv)) => lk <= rk && lv <= rv
  }

  /** union type */
  def ||(that: MapTy): MapTy =
    if (this eq that) this
    else
      (this, that) match
        case (Bot, _) | (_, Top)          => that
        case (Top, _) | (_, Bot)          => this
        case (Elem(lk, lv), Elem(rk, rv)) => Elem(lk || rk, lv || rv).normalized

  /** intersection type */
  def &&(that: MapTy): MapTy =
    if (this eq that) this
    else
      (this, that) match
        case (Bot, _) | (_, Top)          => this
        case (Top, _) | (_, Bot)          => that
        case (Elem(lk, lv), Elem(rk, rv)) => Elem(lk && rk, lv && rv).normalized

  /** prune type */
  def --(that: MapTy): MapTy = if (this <= that) Bot else this

  /** get key type */
  def getKey: ValueTy = this match
    case Top        => AnyT
    case Bot        => BotT
    case Elem(k, _) => k

  /** flatten */
  def toFlat: Flat[Value] = if (this.isBottom) Zero else Many

  /** map containment check */
  def contains(m: MapObj, heap: Heap): Boolean = this match
    case Top        => true
    case Bot        => false
    case Elem(k, v) => m.map.forall(k.contains(_, heap) && v.contains(_, heap))

  /** normalized type */
  def normalized: MapTy = this match
    case Elem(key, value) if key.isTop && value.isTop       => Top
    case Elem(key, value) if key.isBottom || value.isBottom => Bot
    case _                                                  => this
}
object MapTy extends Parser.From(Parser.mapTy) {
  def apply(key: ValueTy, value: ValueTy): MapTy = Elem(key, value).normalized
}
