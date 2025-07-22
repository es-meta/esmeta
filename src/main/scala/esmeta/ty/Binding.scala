package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** field binding types */
case class Binding(
  value: ValueTy,
  absent: Boolean = false,
) extends TyElem
  with Lattice[Binding] {

  /** top check */
  def isTop: Boolean = value.isTop && absent.isTop

  /** bottom check */
  def isBottom: Boolean = value.isBottom && absent.isBottom

  /** Absent check */
  def isAbsent: Boolean = value.isBottom && absent.isTop

  /** partial order/subset operator */
  def <=(that: => Binding): Boolean = (this eq that) || {
    this.value <= that.value &&
    this.absent <= that.absent
  }

  /** union type */
  def ||(that: => Binding): Binding =
    if (this eq that) this
    else
      Binding(
        this.value || that.value,
        this.absent || that.absent,
      )

  /** intersection type */
  def &&(that: => Binding): Binding =
    if (this eq that) this
    else
      Binding(
        this.value && that.value,
        this.absent && that.absent,
      )

  /** prune type */
  def --(that: => Binding): Binding =
    if (that.isBottom) this
    else
      Binding(
        this.value -- that.value,
        this.absent -- that.absent,
      )

  /** existence check */
  def exists: Set[Boolean] =
    (if (!value.isBottom) Set(true) else Set()) ++
    (if (absent) Set(false) else Set())

  /** containment check */
  def contains(v: Option[Value], heap: Heap): Boolean = v match
    case Some(Undef)        => true // Undef represents uninitialized value
    case Some(value: Value) => this.value.contains(value, heap)
    case None               => this.absent
}
object Binding extends Parser.From(Parser.binding) {
  lazy val Top: Binding = Binding(AnyT, true)
  lazy val Exist: Binding = Binding(AnyT, false)
  lazy val Absent: Binding = Binding(BotT, true)
  lazy val Bot: Binding = Binding(BotT, false)
}
