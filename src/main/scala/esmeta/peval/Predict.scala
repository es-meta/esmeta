package esmeta.peval

import esmeta.error.PartialEvaluatorError

sealed trait Predict[+A] extends IterableOnce[A] {
  self =>

  final def isEmpty: Boolean = this eq Unknown
  final def isDefined: Boolean = !isEmpty

  def asKnown: A = if isEmpty then
    throw new PartialEvaluatorError("not known value")
  else get

  override final def knownSize: Int = if (isEmpty) 0 else 1
  def get: A
  @inline final def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else this.get

  @inline final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 =
    this getOrElse ev(null)
  @inline final def map[B](f: A => B): Predict[B] =
    if (isEmpty) Unknown else Known(f(this.get))

  @inline final def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(this.get)

  @inline final def flatMap[B](f: A => Predict[B]): Predict[B] =
    if (isEmpty) Unknown else f(this.get)

  def flatten[B](implicit ev: A <:< Predict[B]): Predict[B] =
    if (isEmpty) Unknown else ev(this.get)

  @inline final def filter(p: A => Boolean): Predict[A] =
    if (isEmpty || p(this.get)) this else Unknown

  @inline final def filterNot(p: A => Boolean): Predict[A] =
    if (isEmpty || !p(this.get)) this else Unknown

  final def Unknownmpty: Boolean = isDefined

  @inline final def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Predict[B] = self filter p map f
    def flatMap[B](f: A => Predict[B]): Predict[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x =>
      p(x) && q(x),
    )
  }

  final def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && this.get == elem

  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  @inline final def forall(p: A => Boolean): Boolean = isEmpty || p(this.get)

  @inline final def foreach[U](f: A => U): Unit = {
    if (!isEmpty) f(this.get)
  }

  @inline final def orElse[B >: A](alternative: => Predict[B]): Predict[B] =
    if (isEmpty) alternative else this

  final def zip[A1 >: A, B](that: Predict[B]): Predict[(A1, B)] =
    if (isEmpty || that.isEmpty) Unknown else Known((this.get, that.get))

  final def unzip[A1, A2](implicit
    asPair: A <:< (A1, A2),
  ): (Predict[A1], Predict[A2]) = {
    if (isEmpty) (Unknown, Unknown)
    else {
      val e = asPair(this.get)
      (Known(e._1), Known(e._2))
    }
  }

  final def unzip3[A1, A2, A3](implicit
    asTriple: A <:< (A1, A2, A3),
  ): (Predict[A1], Predict[A2], Predict[A3]) = {
    if (isEmpty) (Unknown, Unknown, Unknown)
    else {
      val e = asTriple(this.get)
      (Known(e._1), Known(e._2), Known(e._3))
    }
  }

  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty
    else collection.Iterator.single(this.get)

  def toList: List[A] =
    if (isEmpty) List() else new ::(this.get, Nil)

  @inline final def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(this.get)

  @inline final def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(this.get)
}

final case class Known[+A](value: A) extends Predict[A]:
  def get: A = value

case object Unknown extends Predict[Nothing]:
  def get: Nothing = ???
