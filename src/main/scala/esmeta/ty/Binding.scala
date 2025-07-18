package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** field binding types */
case class Binding(
  value: ValueTy,
  uninit: Boolean = false,
  absent: Boolean = false,
) extends TyElem
  with Lattice[Binding] {

  /** top check */
  def isTop: Boolean = value.isTop && uninit.isTop && absent.isTop

  /** bottom check */
  def isBottom: Boolean = value.isBottom && uninit.isBottom && absent.isBottom

  /** Absent check */
  def isAbsent: Boolean = value.isBottom && !uninit && absent

  /** partial order/subset operator */
  def <=(that: => Binding): Boolean = (this eq that) || {
    this.value <= that.value &&
    this.uninit <= that.uninit &&
    this.absent <= that.absent
  }

  /** union type */
  def ||(that: => Binding): Binding =
    if (this eq that) this
    else
      Binding(
        this.value || that.value,
        this.uninit || that.uninit,
        this.absent || that.absent,
      )

  /** intersection type */
  def &&(that: => Binding): Binding =
    if (this eq that) this
    else
      Binding(
        this.value && that.value,
        this.uninit && that.uninit,
        this.absent && that.absent,
      )

  /** prune type */
  def --(that: => Binding): Binding =
    if (that.isBottom) this
    else
      Binding(
        this.value -- that.value,
        this.uninit -- that.uninit,
        this.absent -- that.absent,
      )

  /** existence check */
  def exists: Set[Boolean] =
    (if (!value.isBottom || uninit) Set(true) else Set()) ++
    (if (absent) Set(false) else Set())

  /** containment check */
  def contains(v: Option[Value | Uninit], heap: Heap): Boolean = v match
    case Some(value: Value) => this.value.contains(value, heap)
    case Some(Uninit)       => this.uninit
    case None               => this.absent
}
object Binding extends Parser.From(Parser.binding) {
  lazy val Top: Binding = Binding(AnyT, true, true)
  lazy val Exist: Binding = Binding(AnyT, true, false)
  lazy val Init: Binding = Binding(AnyT, false, false)
  lazy val Uninit: Binding = Binding(BotT, true, false)
  lazy val Absent: Binding = Binding(BotT, false, true)
  lazy val Bot: Binding = Binding(BotT, false, false)
}
