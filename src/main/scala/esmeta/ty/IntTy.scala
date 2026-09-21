package esmeta.ty

import esmeta.util.Lattice
import esmeta.interpreter.{Interpreter, *}
import esmeta.state.{Math, Number}
import esmeta.util.Flat

import scala.math.BigInt

sealed trait IntTy extends TyElem with Lattice[IntTy] {
  import IntTy.*

  def isTop: Boolean = this == Top
  def isBottom: Boolean = this.canon match
    case IntSetTy(set)   => set.isEmpty
    case IntSignTy(sign) => sign.isBottom

  def <=(that: => IntTy): Boolean = (this.canon, that.canon) match
    case (l, r) if (l == r) || l.isBottom || r.isTop => true
    case (IntSetTy(lset), IntSetTy(rset))            => lset subsetOf rset
    case (IntSetTy(lset), IntSignTy(rsign))   => lset.forall(rsign.contains)
    case (IntSignTy(lsign), IntSignTy(rsign)) => lsign <= rsign
    // a finite set never covers an infinite negative or positive component
    case (IntSignTy(lsign), IntSetTy(_)) => lsign.isBottom

  def ||(that: => IntTy): IntTy =
    (this.canon, that.canon) match
      case _ if this eq that                    => this
      case (IntSetTy(lset), IntSetTy(rset))     => IntSetTy(lset union rset)
      case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign || rsign)
      case (l, r) => IntSignTy(l.toSign || r.toSign)

  def &&(that: => IntTy): IntTy = (this, that) match
    case _ if this eq that                    => this
    case (IntSetTy(lset), IntSetTy(rset))     => IntSetTy(lset intersect rset)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign && rsign)
    case (IntSetTy(set), IntSignTy(sign)) => IntSetTy(set.filter(sign.contains))
    case (IntSignTy(sign), IntSetTy(set)) => IntSetTy(set.filter(sign.contains))

  def --(that: => IntTy): IntTy = (this.canon, that.canon) match
    case _ if this eq that                => Bot
    case (IntSetTy(lset), IntSetTy(rset)) => IntSetTy(lset -- rset)
    case (IntSetTy(lset), IntSignTy(rsign)) =>
      IntSetTy(lset.filterNot(rsign.contains))
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign -- rsign)
    // a finite set cannot cover the infinite negative or positive component
    case (IntSignTy(lsign), IntSetTy(rset)) =>
      IntSignTy(Sign(lsign.neg, lsign.zero && !rset(BigInt(0)), lsign.pos))

  def +(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(_), r @ IntSetTy(_)) if single(l, r, _ + _) != Top =>
      single(l, r, _ + _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign + rsign)
    case (l, r)                               => l.toSignTy + r.toSignTy

  def -(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(_), r @ IntSetTy(_)) if single(l, r, _ - _) != Top =>
      single(l, r, _ - _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign - rsign)
    case (l, r)                               => l.toSignTy - r.toSignTy

  def *(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(_), r @ IntSetTy(_)) if single(l, r, _ * _) != Top =>
      single(l, r, _ * _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign * rsign)
    case (l, r)                               => l.toSignTy * r.toSignTy

  def /(that: => IntTy): IntTy = (this.canon, that.canon) match
    case (l, r) if l.isBottom || r.isBottom => Bot
    case (IntSetTy(lset), IntSetTy(rset)) =>
      IntSetTy(for { l <- lset; r <- rset if r != 0 } yield l / r)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign / rsign)
    case (l, r)                               => l.toSignTy / r.toSignTy

  def %(that: => IntTy): IntTy = (this.canon, that.canon) match
    case (l, r) if l.isBottom || r.isBottom => Bot
    // a zero divisor traps, so it contributes nothing to the result
    case (IntSetTy(lset), IntSetTy(rset)) =>
      IntSetTy(for { l <- lset; r <- rset if r != 0 } yield l %% r)
    case (IntSignTy(_), IntSetTy(rset)) if rset.size == 1 =>
      val r = rset.head
      // this is the Euclidean modulo, so the result lies between 0 and r
      if (r > 8) NonNeg
      else if (r < -8) NonPos
      else if (r > 0) IntSetTy((0 until r.toInt).toSet.map(BigInt(_)))
      else if (r < 0) IntSetTy((r.toInt + 1 to 0).toSet.map(BigInt(_)))
      else Bot
    case _ => Top

  def **(that: => IntTy): IntTy =
    import esmeta.util.{One => Single}
    if (this.isBottom || that.isBottom) Bot
    // a negative exponent does not yield an integer
    else if (!that.toSign.isNonNeg) Top
    else
      val exact = (this.getSingle, that.getSingle) match
        // a huge exponent would not fit in memory
        case (Single(l), Single(r)) if r.isValidInt && r <= 1024 =>
          IntSetTy(Set(l.pow(r.toInt)))
        case _ => Top
      if (exact != Top) exact
      else if (this.toSign.isPos) Pos
      else if (this.toSign.isNonNeg) NonNeg
      else Top

  def &(that: => IntTy): IntTy = single(this, that, _ & _)

  def |(that: => IntTy): IntTy = single(this, that, _ | _)

  def ^(that: => IntTy): IntTy = single(this, that, _ ^ _)

  def <<(that: => IntTy): IntTy = shift(that, _ << _)

  def >>(that: => IntTy): IntTy = shift(that, _ >> _)

  /** a shift by a huge amount would not fit in memory, so give up on it */
  private def shift(that: => IntTy, f: (BigInt, Int) => BigInt): IntTy =
    import esmeta.util.{One => Single}
    (this.getSingle, that.getSingle) match
      case (Single(l), Single(r)) if r.isValidInt && r.abs <= 1024 =>
        IntSetTy(Set(f(l, r.toInt)))
      case _ => Top

  def >>>(that: => IntTy): IntTy = Top

  def unary_- : IntTy = this.canon match
    case IntSetTy(set)   => IntSetTy(set.map(-_))
    case IntSignTy(sign) => IntSignTy(-sign)

  def unary_~ : IntTy = this.canon match
    case IntSetTy(set) => IntSetTy(set.map(~_))
    case IntSignTy(sign) =>
      if sign == Sign.Top || sign == Sign.NonPos then Top
      else if sign == Sign.NonNeg || sign == Sign.Pos then IntTy.Neg
      else if sign == Sign.Neg then IntTy.Pos
      else IntTy.Bot

  def abs: IntTy = this.canon match
    case IntSetTy(set)   => IntSetTy(set.map(_.abs))
    case IntSignTy(sign) => IntSignTy(sign.abs)

  // every integer is its own floor
  def floor: IntTy = this.canon

  def min(that: => IntTy): IntTy = (this.canon, that.canon) match
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign min rsign)
    case _ => IntSignTy(this.toSign min that.toSign)

  def max(that: => IntTy): IntTy = (this.canon, that.canon) match
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign max rsign)
    case _ => IntSignTy(this.toSign max that.toSign)

  def contains(value: Int): Boolean = this.canon match
    case IntSetTy(set)   => set.contains(value)
    case IntSignTy(sign) => sign.contains(value)

  def contains(value: BigInt): Boolean = this.canon match
    case IntSetTy(set)   => set.contains(value)
    case IntSignTy(sign) => sign.contains(value)

  def isNonPos: Boolean = this.canon match
    case IntSignTy(sign) => sign.isNonPos
    case s               => s.toSignTy.isNonPos

  def isNonNeg: Boolean = this.canon match
    case IntSignTy(sign) => sign.isNonNeg
    case s               => s.toSignTy.isNonNeg

  def isPos: Boolean = this.canon match
    case IntSignTy(sign) => sign.isPos
    case s               => s.toSignTy.isPos

  def isNeg: Boolean = this.canon match
    case IntSignTy(sign) => sign.isNeg
    case s               => s.toSignTy.isNeg

  def toSign: Sign = this.canon match
    case IntSetTy(set) =>
      Sign.alpha(
        set,
        v =>
          if (v < 0) Sign.Neg
          else if (v > 0) Sign.Pos
          else Sign.Zero,
      )
    case IntSignTy(sign) => sign

  def toSignTy: IntSignTy = this.canon match
    case a @ IntSignTy(_) => a
    case IntSetTy(set)    => IntSignTy(this.toSign)

  def isFinite: Boolean = this.canon match
    case IntSignTy(sign) => sign.isZero
    case IntSetTy(set)   => true

  def getSingle: Flat[BigInt] =
    import esmeta.util.*
    this.canon match
      case IntSetTy(set) => Flat(set)
      case IntSignTy(sign) =>
        if sign.isBottom then Zero
        else if sign.isZero then One(0)
        else Many

  def canon: IntTy = this match
    case s @ IntSetTy(set)                => s
    case IntSignTy(sign) if sign.isBottom => IntSetTy(Set())
    case IntSignTy(sign) if sign.isZero   => IntSetTy(Set(0))
    case IntSignTy(_)                     => this

  def toMathSet: Option[Set[Math]] = this.canon match
    case IntSetTy(set) => Some(set.map(Math(_)))
    case _             => None

  def toNumberSet: Option[Set[Number]] = this.canon match
    // the integral number 0 stands for both *+0* and *-0*
    case IntSetTy(set) =>
      Some(set.flatMap {
        case x if x == 0 => Set(Number(0.0), Number(-0.0))
        case x           => Set(Number(x.toDouble))
      })
    case _ => None
}

case class IntSetTy(set: Set[BigInt]) extends IntTy
case class IntSignTy(sign: Sign) extends IntTy

object IntTy {
  lazy val Top: IntTy = IntSignTy(Sign.Top)
  lazy val Bot: IntTy = IntSetTy(Set())
  lazy val Pos: IntTy = IntSignTy(Sign.Pos)
  lazy val Neg: IntTy = IntSignTy(Sign.Neg)
  lazy val Zero: IntTy = IntSetTy(Set(0))
  lazy val One: IntTy = IntSetTy(Set(1))
  lazy val NonNeg: IntTy = IntSignTy(Sign.NonNeg)
  lazy val NonPos: IntTy = IntSignTy(Sign.NonPos)

  /** This calculates concrete values for the given singletons.
    *
    * @param l
    *   integer value which wants to be a singleton
    * @param r
    *   integer value which wants to be a singleton
    * @param f
    *   function to calculate the result
    * @return
    *   a singleton set if the result is a singleton, otherwise Top
    */
  def single(l: IntTy, r: IntTy, f: (BigInt, BigInt) => BigInt) =
    import esmeta.util
    (l.getSingle, r.getSingle) match
      case (util.One(lv), util.One(rv)) => IntSetTy(Set(f(lv, rv)))
      case _                            => Top
}
