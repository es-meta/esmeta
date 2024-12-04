package esmeta.ty

import esmeta.state.Math
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.interpreter.Interpreter

sealed trait IntTy extends TyElem with Lattice[IntTy] {
  import IntTy.*

  def isTop: Boolean = this == Top
  def isBottom: Boolean = this match
    case IntSetTy(set)   => set.isEmpty
    case IntSignTy(sign) => sign.isBottom

  def <=(that: => IntTy): Boolean = (this, that) match
    case _ if (this == that) || (this == Bot) || (that eq Top) => true
    case (IntSetTy(lset), IntSetTy(rset))     => lset subsetOf rset
    case (IntSignTy(lsign), IntSignTy(rsign)) => lsign <= rsign
    case (l, r)                               => l.toSign <= r.toSign

  def ||(that: => IntTy): IntTy =
    (this, that) match
      case _ if this eq that                    => this
      case (IntSetTy(lset), IntSetTy(rset))     => IntSetTy(lset union rset)
      case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign || rsign)
      case (l, r)                               => l.toSign || r.toSign

  def &&(that: => IntTy): IntTy = (this, that) match
    case _ if this eq that                    => this
    case (IntSetTy(lset), IntSetTy(rset))     => IntSetTy(lset intersect rset)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign && rsign)
    case (l, r)                               => l.toSign && r.toSign

  def --(that: => IntTy): IntTy = (this, that) match
    case _ if this eq that                    => Bot
    case (IntSetTy(lset), IntSetTy(rset))     => IntSetTy(lset -- rset)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign -- rsign)
    case (l, r)                               => l.toSign -- r.toSign

  def +(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(lset), r @ IntSetTy(rset))
        if withSingle(l, r, _ + _) != Top =>
      withSingle(l, r, _ + _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign + rsign)
    case (l, r)                               => l.toSign + r.toSign

  def -(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(lset), r @ IntSetTy(rset))
        if withSingle(l, r, _ - _) != Top =>
      withSingle(l, r, _ - _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign - rsign)
    case (l, r)                               => l.toSign - r.toSign

  def *(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(lset), r @ IntSetTy(rset))
        if withSingle(l, r, _ * _) != Top =>
      withSingle(l, r, _ * _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign * rsign)
    case (l, r)                               => l.toSign * r.toSign

  def /(that: => IntTy): IntTy = (this, that) match
    case (l @ IntSetTy(lset), r @ IntSetTy(rset))
        if withSingle(l, r, _ / _) != Top =>
      withSingle(l, r, _ / _)
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign / rsign)
    case (l, r)                               => l.toSign / r.toSign

  def %(that: => IntTy): IntTy =
    import esmeta.util.*
    (this, that) match
      case (IntSetTy(lset), IntSetTy(rset))
          if withSingle(this, that, _ % _) != Top =>
        withSingle(this, that, _ % _)
      case _ => Top

  def **(that: => IntTy): IntTy =
    withSingle(this, that, (l, r) => l.pow(r.toInt))

  def &(that: => IntTy): IntTy = withSingle(this, that, _ & _)

  def |(that: => IntTy): IntTy = withSingle(this, that, _ | _)

  def ^(that: => IntTy): IntTy = withSingle(this, that, _ ^ _)

  def <<(that: => IntTy): IntTy = withSingle(this, that, _ << _.toInt)

  def >>(that: => IntTy): IntTy = withSingle(this, that, _ >> _.toInt)

  def >>>(that: => IntTy): IntTy = Top

  def unary_- : IntTy = this match
    case IntSetTy(set)   => IntSetTy(set.map(-_))
    case IntSignTy(sign) => IntSignTy(-sign)

  def unary_~ : IntTy = this match
    case IntSetTy(set) => IntSetTy(set.map(~_))
    case IntSignTy(sign) =>
      if sign == Sign.Top || sign == Sign.NonPos then Top
      else if sign == Sign.NonNeg || sign == Sign.Pos then IntTy.Neg
      else if sign == Sign.Neg then IntTy.Pos
      else IntTy.Bot

  def abs: IntTy = this match
    case IntSetTy(set)   => IntSetTy(set.map(_.abs))
    case IntSignTy(sign) => IntSignTy(sign.abs)

  def floor: IntTy = this match
    case IntSetTy(set) =>
      IntSetTy(set.map(x => Interpreter.floor(Math(x)).toInt))
    case IntSignTy(sign) => IntSignTy(sign)

  def min(that: => IntTy): IntTy = (this, that) match
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign min rsign)
    case _                                    => this.toSign min that.toSign

  def max(that: => IntTy): IntTy = (this, that) match
    case (IntSignTy(lsign), IntSignTy(rsign)) => IntSignTy(lsign max rsign)
    case _                                    => this.toSign max that.toSign

  def contains(value: Math): Boolean = this match
    case IntSetTy(set)   => value.decimal.isWhole && set.contains(value.toInt)
    case IntSignTy(sign) => value.decimal.isWhole && sign.contains(value.toInt)

  def isNonPos: Boolean = this match
    case IntSignTy(sign) => sign.isNonPos
    case s               => s.toSign.isNonPos

  def isNonNeg: Boolean = this match
    case IntSignTy(sign) => sign.isNonNeg
    case s               => s.toSign.isNonNeg

  def isPos: Boolean = this match
    case IntSignTy(sign) => sign.pos
    case s               => s.toSign.isPos

  def isNeg: Boolean = this match
    case IntSignTy(sign) => sign.neg
    case s               => s.toSign.isNeg

  def toSign: IntSignTy = this match
    case a @ IntSignTy(_) => a
    case IntSetTy(set) =>
      IntSignTy(
        Sign.alpha(
          set,
          v =>
            if (v < 0) Sign.Neg
            else if (v > 0) Sign.Pos
            else Sign.Zero,
        ),
      )

  def isFinite: Boolean = this match
    case IntSignTy(sign) => sign.isZero
    case IntSetTy(set)   => true

  def getSingle: Flat[BigInt] =
    import esmeta.util.*
    this match
      case IntSetTy(set) => Flat(set)
      case IntSignTy(sign) =>
        if sign.isBottom then Zero
        else if sign.isZero then One(0)
        else Many

  def toMathSet: Option[Set[Math]] = this match
    case IntSetTy(set) => Some(set.map(Math(_)))
    case _             => None
}

object IntTy {
  lazy val Top: IntTy = IntSignTy(Sign.Top)
  lazy val Bot: IntTy = IntSignTy(Sign.Bot)
  lazy val Pos: IntTy = IntSignTy(Sign.Pos)
  lazy val Neg: IntTy = IntSignTy(Sign.Neg)
  lazy val Zero: IntTy = IntSetTy(Set(0))
  lazy val One: IntTy = IntSetTy(Set(1))
  lazy val NonNeg: IntTy = IntSignTy(Sign.NonNeg)
  lazy val NonPos: IntTy = IntSignTy(Sign.NonPos)

  def withSingle(l: IntTy, r: IntTy, f: (BigInt, BigInt) => BigInt) =
    import esmeta.util
    (l.getSingle, r.getSingle) match
      case (util.One(lv), util.One(rv)) => IntSetTy(Set(f(lv, rv)))
      case _                            => Top
}

case class IntSetTy(set: Set[BigInt]) extends IntTy
case class IntSignTy(sign: Sign) extends IntTy

/** mathematical value types */
sealed trait MathTy extends TyElem with Lattice[MathTy] {
  import MathTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this match
    case MathSignTy(sign) => sign.isBottom
    case MathSetTy(set)   => set.isEmpty
    case MathIntTy(int)   => int.isBottom

  private def integrate(int: IntTy, set: Set[Math])(
    f: (Set[Math], Set[Math]) => Set[Math],
  ): MathTy =
    int.toMathSet.fold(Top) {
      case mset => MathSetTy(f(mset, set))
    }
  private def integrate(set: Set[Math], int: IntTy)(
    f: (Set[Math], Set[Math]) => Set[Math],
  ): MathTy =
    int.toMathSet.fold(Top) {
      case mset => MathSetTy(f(set, mset))
    }

  /** Partial order. In default, IntSignTy is a subset of the MathSignTy. We
    * cannot ensure other orders, unless we can directly compare them if some of
    * them are SetTy.
    */
  def <=(that: => MathTy): Boolean = (this.norm, that.norm) match
    case _ if (this eq that) || (this == Bot) => true
    // same types
    case (MathSignTy(l), MathSignTy(r))     => l <= r
    case (MathIntTy(l), MathIntTy(r))       => l <= r
    case (MathSetTy(lset), MathSetTy(rset)) => lset subsetOf rset
    // subset
    case (MathIntTy(l), MathSignTy(r)) => l.toSign.sign <= r
    // comparsion with set
    case (MathIntTy(int), MathSetTy(set)) =>
      int.toMathSet.fold(false) {
        case mset => mset subsetOf set
      }
    case (MathSetTy(set), MathIntTy(int))     => false
    case (l @ MathSetTy(lset), MathSignTy(r)) => l.toSign.sign <= r
    case _                                    => false

  /** union type */
  def ||(that: => MathTy): MathTy = (this.norm, that.norm) match
    case _ if this eq that            => this
    case (l, r) if l.isTop || r.isTop => Top
    // same types
    case (MathSignTy(l), MathSignTy(r))     => MathSignTy(l || r)
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l || r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset union rset)
    // comparison with set
    case (MathIntTy(int), MathSetTy(set)) => integrate(int, set)(_ union _)
    case (MathSetTy(set), MathIntTy(int)) => integrate(set, int)(_ union _)
    case (l, r) => MathSignTy(l.toSign.sign || r.toSign.sign)

  /** intersection type */
  def &&(that: => MathTy): MathTy = (this.norm, that.norm) match
    case _ if this eq that                  => this
    case (l, r) if l.isBottom || r.isBottom => Bot
    // same types
    case (MathSignTy(l), MathSignTy(r))     => MathSignTy(l && r)
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l && r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset intersect rset)
    // comparison with set
    case (MathIntTy(int), MathSetTy(set)) => integrate(int, set)(_ intersect _)
    case (MathSetTy(set), MathIntTy(int)) => integrate(set, int)(_ intersect _)
    case (l, r) => MathSignTy(l.toSign.sign && r.toSign.sign)

  /** prune type */
  def --(that: => MathTy): MathTy = (this.norm, that.norm) match
    case _ if this eq that               => Bot
    case (l, r) if l.isBottom || r.isTop => Bot
    // same types
    case (MathSignTy(l), MathSignTy(r))     => MathSignTy(l -- r)
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l -- r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset -- rset)
    // comparison with set
    case (MathIntTy(int), MathSetTy(set)) => integrate(int, set)(_ -- _)
    case (MathSetTy(set), MathIntTy(int)) => integrate(set, int)(_ -- _)
    case (l, r) => MathSignTy(l.toSign.sign -- r.toSign.sign)

  /** addition */
  def +(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l + r)
    case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l + r)
    case _                              => Top

  /** subtraction */
  def -(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l - r)
    case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l - r)
    case _                              => Top

  /** multiplcation */
  def *(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l * r)
    case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l * r)
    case _                              => Top

  /** modulo */
  def %(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l % r)
    case _                            => Top

  /** exponentiation */
  def **(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l ** r)
    case _                            => Top

  /** bitwise operation (&) */
  def &(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l & r)
    case _                            => Top

  /** bitwise operation (|) */
  def |(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l | r)
    case _                            => Top

  /** bitwise operation (^) */
  def ^(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l ^ r)
    case _                            => Top

  /** shift left */
  def <<(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l << r)
    case _                            => Top

  /** shift right */
  def >>(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l >> r)
    case _                            => Top

  /** unsigned shift right */
  def >>>(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l >>> r)
    case _                            => Top

  /** min operation */
  def min(that: MathTy): MathTy =
    import Math.given
    (this.norm, that.norm) match
      case (l, r) if l.isTop || r.isTop   => Top
      case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l min r)
      case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l min r)
      case (MathSetTy(lset), MathSetTy(rset)) =>
        if lset.max < rset.min then MathSetTy(lset)
        else if rset.max < lset.min then MathSetTy(rset)
        else Top
      case _ => Top

  /** max operation */
  def max(that: MathTy): MathTy =
    import Math.given
    (this.norm, that.norm) match
      case (l, r) if l.isTop || r.isTop   => Top
      case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l max r)
      case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l max r)
      case (MathSetTy(lset), MathSetTy(rset)) =>
        if lset.min > rset.max then MathSetTy(lset)
        else if rset.min > lset.max then MathSetTy(rset)
        else Top
      case _ => Top

  /** inclusion check */
  def contains(math: Math): Boolean = this.norm match
    case MathSignTy(sign) => sign.contains(math.decimal)
    case MathIntTy(int)   => int.contains(math)
    case MathSetTy(set)   => set.contains(math)

  /** get single value */
  def getSingle: Flat[Math] =
    this.norm match
      case MathSignTy(sign) => if sign.isZero then Flat(Math(0)) else Many
      case MathIntTy(int)   => int.toMathSet.fold(esmeta.util.Zero)(Flat(_))
      case MathSetTy(set)   => Flat(set)

  /** integral check */
  def isInt: Boolean = this.norm match
    case MathIntTy(_) => true
    case _            => false

  /** non-positive check */
  def isNonPos: Boolean = this.norm match
    case MathSignTy(sign) => sign.isNonPos
    case MathIntTy(int)   => int.isNonPos
    case s                => s.toSign.isNonPos

  /** non-negative check */
  def isNonNeg: Boolean = this.norm match
    case MathSignTy(sign) => sign.isNonNeg
    case MathIntTy(int)   => int.isNonNeg
    case s                => s.toSign.isNonNeg

  /** negative check */
  def isNeg: Boolean = this.norm match
    case MathSignTy(sign) => sign.isNeg
    case MathIntTy(int)   => int.isNeg
    case s                => s.toSign.isNeg

  /** positive check */
  def isPos: Boolean = this.norm match
    case MathSignTy(sign) => sign.isPos
    case MathIntTy(int)   => int.isPos
    case s                => s.toSign.isPos

  /** non-positive integral check */
  def isNonPosInt: Boolean = this.norm match
    case MathIntTy(int) => int.isNonPos
    case _              => false

  /** non-negative integral check */
  def isNonNegInt: Boolean = this.norm match
    case MathIntTy(int) => int.isNonNeg
    case _              => false

  /** negative integral check */
  def isNegInt: Boolean = this.norm match
    case MathIntTy(int) => int.isNeg
    case _              => false

  /** positive integral check */
  def isPosInt: Boolean = this.norm match
    case MathIntTy(int) => int.isPos
    case _              => false

  /** to list of atomic math types */
  def toAtomicTys: List[MathTy] = this match
    case MathSetTy(set) =>
      set
        .map(n =>
          if (n.decimal.isWhole)
            if (n.decimal < 0) NegInt
            else if (n.decimal > 0) PosInt
            else MathTy.Zero
          else Top,
        )
        .toList
    case _ => List(this)

  def norm: MathTy
  def toSign: MathSignTy
}

/** mathematical value types */
case class MathSignTy(sign: Sign) extends MathTy {
  def norm: MathTy =
    if sign.isZero then MathIntTy(IntTy.Zero)
    else if sign.isBottom then MathIntTy(IntTy.Bot)
    else this
  def toSign: MathSignTy = this
}

/** types for set of mathematical values */
case class MathSetTy(set: Set[Math]) extends MathTy {

  private def toInt: Option[IntSetTy] = if (set.forall(_.decimal.isWhole))
    Some(IntSetTy(set.map(_.toInt)))
  else None

  /** This auxiliary method normalize a set if the set contains only integers */
  def norm: MathTy = this.toInt match
    case Some(IntSetTy(set)) => MathIntTy(IntSetTy(set))
    case None                => this

  /** This auxiliary method convert a set to the sign domain */
  def toSign: MathSignTy = MathSignTy(
    Sign.alpha(
      set,
      v =>
        if v.decimal < 0 then Sign.Neg
        else if v.decimal > 0 then Sign.Pos
        else Sign.Zero,
    ),
  )
}
case class MathIntTy(int: IntTy) extends MathTy {
  def norm: MathTy = this
  def isFinite: Boolean = int.isFinite
  def toSign: MathSignTy = MathSignTy(int.toSign.sign)
}

object SingleTy {
  def unapply(ty: MathSetTy): Option[Math] =
    if (ty.set.size == 1) Some(ty.set.head) else None
}
object MathSetTy {
  def apply(seq: Math*): MathSetTy = MathSetTy(seq.toSet)
}

object MathTy extends Parser.From(Parser.mathTy) {
  lazy val Top: MathTy = MathSignTy(Sign.Top)
  lazy val Bot: MathTy = MathSignTy(Sign.Bot)
  lazy val Int: MathTy = MathIntTy(IntTy.Top)
  lazy val NonPosInt: MathTy = MathIntTy(IntTy.NonPos)
  lazy val NonNegInt: MathTy = MathIntTy(IntTy.NonNeg)
  lazy val NegInt: MathTy = MathIntTy(IntTy.Neg)
  lazy val PosInt: MathTy = MathIntTy(IntTy.Pos)
  lazy val NonPos: MathTy = MathSignTy(Sign.NonPos)
  lazy val NonNeg: MathTy = MathSignTy(Sign.NonNeg)
  lazy val Neg: MathTy = MathSignTy(Sign.Neg)
  lazy val Pos: MathTy = MathSignTy(Sign.Pos)
  lazy val Zero: MathTy = MathIntTy(IntTy.Zero)
  lazy val One: MathTy = MathIntTy(IntTy.One)
}
