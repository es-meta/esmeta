package esmeta.util

/** lattice */
trait Lattice[L <: Lattice[L]] {

  /** bottom check */
  def isBottom: Boolean

  /** partial order/subset operator */
  def <=(that: => L): Boolean
  def ⊑(that: => L): Boolean = this <= that

  /** join/union operator */
  def ||(that: => L): L
  def ⊔(that: => L): L = this || that

  /** meet/intersection operator */
  def &&(that: => L): L
  def ⊓(that: => L): L = this && that

  /** prune operator */
  def --(that: => L): L
}

// -----------------------------------------------------------------------------
// simple lattice
// -----------------------------------------------------------------------------
case class Simple(exist: Boolean = false) extends Lattice[Simple] {

  /** top check */
  def isTop: Boolean = exist == true

  /** bottom check */
  def isBottom: Boolean = exist == false

  /** partial order/subset operator */
  def <=(that: => Simple): Boolean = this.exist <= that.exist

  /** join/union operator */
  def ||(that: => Simple): Simple = Simple(this.exist || that.exist)

  /** meet/intersection operator */
  def &&(that: => Simple): Simple = Simple(this.exist && that.exist)

  /** prune operator */
  def --(that: => Simple): Simple = Simple(this.exist && !that.exist)
}
