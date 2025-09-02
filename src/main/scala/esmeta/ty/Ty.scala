package esmeta.ty

import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.BaseUtils.*

/** types */
trait Ty extends TyElem {

  /** definite type check */
  def isDefined: Boolean = this match
    case _: ValueTy => true
    case _          => false

  /** imprecise type check */
  def isImprec: Boolean = this match
    case ty: ValueTy => AbruptT <= ty || NormalT <= ty || AstT <= ty
    case _           => true

  /** completion check */
  def isCompletion: Boolean

  /** conversion to value type */
  def toValue: ValueTy = this match
    case ty: ValueTy => ty
    case _           => AnyT

  /** value containment check */
  def contains(value: Value, st: State): Boolean = contains(value, st.heap)

  /** value containment check */
  def contains(value: Value, heap: Heap): Boolean

  def containsNonAddr(value: Value): Option[Boolean]

  /** safe value containment check */
  def safeContains(value: Value, st: State): Option[Boolean] =
    safeContains(value, st.heap)

  /** safe value containment check */
  def safeContains(value: Value, heap: Heap): Option[Boolean] =
    optional(contains(value, heap))
}
object Ty extends Parser.From(Parser.ty)
