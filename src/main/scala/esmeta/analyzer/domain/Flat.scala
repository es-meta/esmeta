package esmeta.analyzer.domain

/** flat lattice */
sealed trait Flat[+T] {
  // join operator
  def ⊔[U >: T](that: => Flat[U]): Flat[U] = this match
    case FlatBot => that
    case FlatTop => FlatTop
    case FlatElem(elem) =>
      that match
        case FlatBot => this
        case _       => FlatTop

  // map function
  def map[B](f: T => B): Flat[B] = this match
    case FlatBot        => FlatBot
    case FlatElem(elem) => FlatElem(f(elem))
    case FlatTop        => FlatTop
}
case object FlatBot extends Flat[Nothing]
case object FlatTop extends Flat[Nothing]
case class FlatElem[T](elem: T) extends Flat[T]
