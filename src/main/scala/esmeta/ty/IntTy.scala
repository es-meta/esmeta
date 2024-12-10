package esmeta.ty

import esmeta.util.Lattice
import esmeta.interpreter.Interpreter
import esmeta.state.{Math, Number}
import esmeta.util.Flat

sealed trait IntTy extends TyElem with Lattice[IntTy] {
  import IntTy.*

  def isTop: Boolean = this == Top
  def isBottom: Boolean = this.canon match
    case IntSetTy(set)   => set.isEmpty
    case IntSignTy(sign) => sign.isBottom

  def <=(that: => IntTy): Boolean = (this, that) match
    case _ if (this == that) || (this == Bot) || (that eq Top) => true
    case (IntSetTy(lset), IntSetTy(rset))     => lset subsetOf rset
    case (IntSignTy(lsign), IntSignTy(rsign)) => lsign <= rsign
    case (l, r)                               => l.toSign <= r.toSign

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
    case (l, r)                               => IntSignTy(l.toSign && r.toSign)

  def --(that: => IntTy): IntTy = (this, that) match
    case _ if this eq that                    => Bot
    case (IntSetTy(lset), IntSetTy(rset))     => IntSetTy(lset -- rset)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign -- rsign)
    case (l, r)                               => IntSignTy(l.toSign -- r.toSign)

  def +(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(lset), r @ IntSetTy(rset))
        if single(l, r, _ + _) != Top =>
      single(l, r, _ + _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign + rsign)
    case (l, r)                               => l.toSignTy + r.toSignTy

  def -(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(lset), r @ IntSetTy(rset))
        if single(l, r, _ - _) != Top =>
      single(l, r, _ - _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign - rsign)
    case (l, r)                               => l.toSignTy - r.toSignTy

  def *(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(lset), r @ IntSetTy(rset))
        if single(l, r, _ * _) != Top =>
      single(l, r, _ * _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign * rsign)
    case (l, r)                               => l.toSignTy * r.toSignTy

  def /(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(lset), r @ IntSetTy(rset))
        if single(l, r, _ / _) != Top =>
      single(l, r, _ / _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign / rsign)
    case (l, r)                               => l.toSignTy / r.toSignTy

  def %(that: => IntTy): IntTy =
    import esmeta.util.*
    (this, that) match
      case (IntSetTy(lset), IntSetTy(rset))
          if single(this, that, _ % _) != Top =>
        single(this, that, _ % _)
      case _ => Top

  def **(that: => IntTy): IntTy =
    single(this, that, (l, r) => scala.math.pow(l.toDouble, r.toDouble).toLong)

  def &(that: => IntTy): IntTy = single(this, that, _ & _)

  def |(that: => IntTy): IntTy = single(this, that, _ | _)

  def ^(that: => IntTy): IntTy = single(this, that, _ ^ _)

  def <<(that: => IntTy): IntTy = single(this, that, _ << _.toInt)

  def >>(that: => IntTy): IntTy = single(this, that, _ >> _.toInt)

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

  def floor: IntTy = this.canon match
    case IntSetTy(set) =>
      IntSetTy(set.map(x => Interpreter.floor(Math(x)).toInt))
    case IntSignTy(sign) => IntSignTy(sign)

  def min(that: => IntTy): IntTy = (this.canon, that.canon) match
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign min rsign)
    case _ => IntSignTy(this.toSign min that.toSign)

  def max(that: => IntTy): IntTy = (this.canon, that.canon) match
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign max rsign)
    case _ => IntSignTy(this.toSign max that.toSign)

  def contains(value: Int): Boolean = this.canon match
    case IntSetTy(set)   => set.contains(value)
    case IntSignTy(sign) => sign.contains(value)

  def isNonPos: Boolean = this.canon match
    case IntSignTy(sign) => sign.isNonPos
    case s               => s.toSignTy.isNonPos

  def isNonNeg: Boolean = this.canon match
    case IntSignTy(sign) => sign.isNonNeg
    case s               => s.toSignTy.isNonNeg

  def isPos: Boolean = this.canon match
    case IntSignTy(sign) => sign.pos
    case s               => s.toSignTy.isPos

  def isNeg: Boolean = this.canon match
    case IntSignTy(sign) => sign.neg
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

  def getSingle: Flat[Long] =
    import esmeta.util.*
    this.canon match
      case IntSetTy(set) => Flat(set)
      case IntSignTy(sign) =>
        if sign.isBottom then Zero
        else if sign.isZero then One(0)
        else Many

  def canon: IntTy = this match
    case s @ IntSetTy(set)              => s
    case IntSignTy(sign) if sign.isZero => IntSetTy(Set(0))
    case IntSignTy(_)                   => this

  def toMathSet: Option[Set[Math]] = this.canon match
    case IntSetTy(set) => Some(set.map(Math(_)))
    case _             => None

  def toNumberSet: Option[Set[Number]] = this.canon match
    case IntSetTy(set) => Some(set.map(x => Number(x.toDouble)))
    case _             => None
}

case class IntSetTy(set: Set[Long]) extends IntTy
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
  def single(l: IntTy, r: IntTy, f: (Long, Long) => Long) =
    import esmeta.util
    (l.getSingle, r.getSingle) match
      case (util.One(lv), util.One(rv)) => IntSetTy(Set(f(lv, rv)))
      case _                            => Top
}
