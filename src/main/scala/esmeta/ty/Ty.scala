package esmeta.ty

import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.peval.{Predict}
import esmeta.peval.pstate.{PState, PHeap}
import esmeta.ty.util.Parser

import scala.collection.mutable.{Map as MMap}

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
  def contains(pv: Predict[Value], pst: PState): Predict[Boolean] =
    contains(pv, pst.heap)
  def contains(value: Value, heap: Heap): Boolean
  def contains(value: Predict[Value], pheap: PHeap): Predict[Boolean]
}
object Ty extends Parser.From(Parser.ty)
