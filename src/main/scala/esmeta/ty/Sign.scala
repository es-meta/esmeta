package esmeta.ty

import esmeta.util.Lattice
import esmeta.ty.util.Parser

case class Sign(neg: Boolean, zero: Boolean, pos: Boolean)
  extends Lattice[Sign] {
  import Sign.*
  def isBottom: Boolean = this == Bot
  def isTop: Boolean = this == Top

  def <=(that: => Sign): Boolean = (this, that) match
    case _ if this eq that => true
    case (Sign(ln, lz, lp), Sign(rn, rz, rp)) =>
      (!ln || rn) && (!lz || rz) && (!lp || rp)

  def ||(that: => Sign): Sign = (this, that) match
    case _ if this eq that => this
    case (Sign(ln, lz, lp), Sign(rn, rz, rp)) =>
      Sign(ln || rn, lz || rz, lp || rp)

  def &&(that: => Sign): Sign = (this, that) match
    case _ if this eq that => this
    case (Sign(ln, lz, lp), Sign(rn, rz, rp)) =>
      Sign(ln && rn, lz && rz, lp && rp)

  def --(that: => Sign): Sign = (this, that) match
    case _ if this eq that => Bot
    case (Sign(ln, lz, lp), Sign(rn, rz, rp)) =>
      Sign(ln && !rn, lz && !rz, lp && !rp)

  def +(that: => Sign): Sign = arithAlpha(this, that, (l, r) => Some(l + r))

  def -(that: => Sign): Sign = arithAlpha(this, that, (l, r) => Some(l - r))

  def *(that: => Sign): Sign = arithAlpha(this, that, (l, r) => Some(l * r))

  def /(that: => Sign): Sign =
    arithAlpha(this, that, (l, r) => if r != 0 then Some(l / r) else None)

  def unary_- : Sign = Sign(pos, zero, neg)

  def abs: Sign = Sign(false, zero, neg || pos)

  def min(that: => Sign): Sign = arithAlpha(this, that, (l, r) => Some(l min r))

  def max(that: => Sign): Sign = arithAlpha(this, that, (l, r) => Some(l max r))

  def contains(value: Int): Boolean =
    if value == 0 then zero
    else if value < 0 then neg
    else pos

  def contains(value: Double): Boolean =
    if value < 0 then neg
    else if value > 0 then pos
    else zero

  def contains(value: BigDecimal): Boolean =
    if value == 0 then zero
    else if value < 0 then neg
    else pos

  def isZero: Boolean = this match
    case Sign(false, true, false) => true
    case _                        => false

  def isPos: Boolean = this match
    case Sign(false, false, _) => true
    case _                     => false

  def isNeg: Boolean = this match
    case Sign(_, false, false) => true
    case _                     => false

  def isNonNeg: Boolean = this match
    case Sign(false, _, _) => true
    case _                 => false

  def isNonPos: Boolean = this match
    case Sign(_, _, false) => true
    case _                 => false
}

object Sign {
  lazy val Top = Sign(true, true, true)
  lazy val Bot = Sign(false, false, false)
  lazy val Pos = Sign(false, false, true)
  lazy val Neg = Sign(true, false, false)
  lazy val Zero = Sign(false, true, false)
  lazy val NonNeg = Sign(false, true, true)
  lazy val NonPos = Sign(true, true, false)

  def alpha[A](l: Iterable[A], f: A => Sign): Sign =
    l.foldLeft[Sign](Bot)((acc, a) => acc || f(a))

  /*
   * This helper works if the operation on {-2, -1, 0, 1, 2} is equivalent on the given domain.
   */
  def arithAlpha(
    l: Sign,
    r: Sign,
    f: (Double, Double) => Option[Double],
  ): Sign =
    def aux(t: Sign): Set[Double] =
      (if t.neg then Set(-1.0, -2.0) else Set.empty) ++
      (if t.zero then Set(0.0) else Set.empty) ++
      (if t.pos then Set(1.0, 2.0) else Set.empty)
    val lset = aux(l)
    val rset = aux(r)
    alpha(
      lset.flatMap(l => rset.map(r => f(l, r))),
      v =>
        v match
          case None => Bot
          case Some(value) =>
            if value < 0 then Neg else if value > 0 then Pos else Zero,
    )
}
