package esmeta.ty

import esmeta.cfg.CFG
import esmeta.error.NotSupported
import esmeta.error.NotSupported.Category.Type
import esmeta.peval.{Predict}
import esmeta.peval.pstate.{PHeap}
import esmeta.state.*
import esmeta.ty.util.*

/** unknown type */
case class UnknownTy(msg: Option[String] = None) extends Ty {

  /** completion check */
  def isCompletion: Boolean = msg.exists(_ contains "Completion")

  /** value containment check */
  def contains(value: Value, heap: Heap): Boolean =
    throw NotSupported(Type)(msg.toList)

  def contains(value: Predict[Value], heap: PHeap): Predict[Boolean] =
    throw NotSupported(Type)(msg.toList)
}
object UnknownTy extends Parser.From(Parser.unknownTy):
  def apply(str: String): UnknownTy = UnknownTy(Some(str))
