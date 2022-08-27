package esmeta.util

/** flat lattice */
sealed trait Flat[+A] {

  /** join operator */
  def ⊔[B >: A](that: => Flat[B]): Flat[B] = (this, that) match
    case (Zero, _) | (_, Many) => that
    case (_, Zero) | (Many, _) => this
    case (One(l), One(r))      => if (l == r) this else Many

  /** map function */
  def map[B](f: A => B): Flat[B] = this match
    case Zero      => Zero
    case One(elem) => One(f(elem))
    case Many      => Many
}

/** more than two elements */
case object Many extends Flat[Nothing]

/** single element */
case class One[A](elem: A) extends Flat[A]

/** no element */
case object Zero extends Flat[Nothing]
