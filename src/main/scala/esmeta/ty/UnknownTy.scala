package esmeta.ty

import esmeta.state.*

/** unknown type */
case class UnknownTy(msg: Option[String] = None) extends Ty {

  /** completion check */
  def isCompletion: Boolean = msg.exists(_ contains "Completion")

  /** value containment check */
  def contains(value: Value, heap: Heap): Boolean = true
}
object UnknownTy:
  def apply(str: String): UnknownTy = UnknownTy(Some(str))
