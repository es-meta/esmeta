package esmeta.ty

import esmeta.state.Math
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.interpreter.Interpreter

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

  def isZero: Boolean = this match
    case Sign(_, true, _) => true
    case _                => false
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
    case IntSignTy(sign) => sign.neg || sign.zero
    case IntSetTy(set)   => set.forall(_ <= 0)

  def isNonNeg: Boolean = this match
    case IntSignTy(sign) => sign.pos || sign.zero
    case IntSetTy(set)   => set.forall(_ >= 0)

  def isPos: Boolean = this match
    case IntSignTy(sign) => sign.pos
    case IntSetTy(set)   => set.forall(_ > 0)

  def isNeg: Boolean = this match
    case IntSignTy(sign) => sign.neg
    case IntSetTy(set)   => set.forall(_ < 0)

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
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean = this match
    case MathTopTy      => false
    case MathSetTy(set) => set.isEmpty
    case MathIntTy(int) => int.isBottom

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

  /** partial order/subset operator */
  def <=(that: => MathTy): Boolean = (this.norm, that.norm) match
    case _ if (this eq that) || (this == Bot) => true
    case (_, MathTopTy)                       => true
    case (MathTopTy, _)                       => false
    case (MathIntTy(l), MathIntTy(r))         => l <= r
    case (MathSetTy(lset), MathSetTy(rset))   => lset subsetOf rset
    case (MathSetTy(set), MathIntTy(int))     => false
    case (MathIntTy(int), MathSetTy(set)) =>
      int.toMathSet.fold(false) {
        case mset => mset subsetOf set
      }

  /** union type */
  def ||(that: => MathTy): MathTy = (this.norm, that.norm) match
    case _ if this eq that                  => this
    case (MathTopTy, _) | (_, MathTopTy)    => Top
    case (MathIntTy(int), MathSetTy(set))   => integrate(int, set)(_ union _)
    case (MathSetTy(set), MathIntTy(int))   => integrate(set, int)(_ union _)
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l || r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset union rset)

  /** intersection type */
  def &&(that: => MathTy): MathTy = (this.norm, that.norm) match
    case _ if this eq that                  => this
    case (_, MathTopTy)                     => this
    case (MathTopTy, _)                     => that
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l && r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset intersect rset)
    case (MathIntTy(int), MathSetTy(set)) => integrate(int, set)(_ intersect _)
    case (MathSetTy(set), MathIntTy(int)) => integrate(set, int)(_ intersect _)

  /** prune type */
  def --(that: => MathTy): MathTy = (this.norm, that.norm) match
    case _ if this eq that                  => Bot
    case (_, MathTopTy)                     => Bot
    case (MathTopTy, _)                     => Top
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l -- r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset -- rset)
    case (MathIntTy(int), MathSetTy(set))   => integrate(int, set)(_ -- _)
    case (MathSetTy(set), MathIntTy(int))   => integrate(set, int)(_ -- _)

  /** addition */
  def +(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l + r)
    case _                            => Top

  /** subtraction */
  def -(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l - r)
    case _                            => Top

  /** multiplcation */
  def *(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l * r)
    case _                            => Top

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
    case _                            => MathTopTy

  /** bitwise operation (|) */
  def |(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l | r)
    case _                            => MathTopTy

  /** bitwise operation (^) */
  def ^(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l ^ r)
    case _                            => MathTopTy

  /** shift left */
  def <<(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l << r)
    case _                            => MathTopTy

  /** shift right */
  def >>(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l >> r)
    case _                            => MathTopTy

  /** unsigned shift right */
  def >>>(that: MathTy): MathTy = (this.norm, that.norm) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l >>> r)
    case _                            => MathTopTy

  /** min operation */
  def min(that: MathTy): MathTy =
    import Math.given
    (this.norm, that.norm) match
      case (MathTopTy, _) | (_, MathTopTy) => MathTopTy
      case (MathIntTy(l), MathIntTy(r))    => MathIntTy(l min r)
      case (MathSetTy(lset), MathSetTy(rset)) =>
        if lset.max < rset.min then MathSetTy(lset)
        else if rset.max < lset.min then MathSetTy(rset)
        else Top
      case _ => Top

  /** max operation */
  def max(that: MathTy): MathTy =
    import Math.given
    (this.norm, that.norm) match
      case (MathTopTy, _) | (_, MathTopTy) => MathTopTy
      case (MathIntTy(l), MathIntTy(r))    => MathIntTy(l max r)
      case (MathSetTy(lset), MathSetTy(rset)) =>
        if lset.min > rset.max then MathSetTy(lset)
        else if rset.min > lset.max then MathSetTy(rset)
        else Top
      case _ => Top

  /** inclusion check */
  def contains(math: Math): Boolean = this.norm match
    case MathTopTy      => true
    case MathIntTy(int) => int.contains(math)
    case MathSetTy(set) => set.contains(math)

  /** get single value */
  def getSingle: Flat[Math] =
    this.norm match
      case MathTopTy      => Many
      case MathIntTy(int) => int.toMathSet.fold(esmeta.util.Zero)(Flat(_))
      case MathSetTy(set) => Flat(set)

  /** integral check */
  def isInt: Boolean = this.norm match
    case MathIntTy(_) => true
    case _            => false

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
            if (n.decimal < 0) MathTy.NegInt
            else if (n.decimal > 0) MathTy.PosInt
            else MathTy.Zero
          else MathTopTy,
        )
        .toList
    case _ => List(this)

  def norm: MathTy
}

/** mathematical value types */
case object MathTopTy extends MathTy {
  def norm: MathTy = this
}

/** types for set of mathematical values */
case class MathSetTy(set: Set[Math]) extends MathTy {
  def toInt: Option[IntSetTy] = if (set.forall(_.decimal.isWhole))
    Some(IntSetTy(set.map(_.toInt)))
  else None
  def norm: MathTy = this.toInt match
    case Some(IntSetTy(set)) => MathIntTy(IntSetTy(set))
    case None                => this
}
case class MathIntTy(int: IntTy) extends MathTy {
  def norm: MathTy = this
  def isFinite: Boolean = int.isFinite
}

object SingleTy {
  def unapply(ty: MathSetTy): Option[Math] =
    if (ty.set.size == 1) Some(ty.set.head) else None
}
object MathSetTy {
  def apply(seq: Math*): MathSetTy = MathSetTy(seq.toSet)
}

object MathTy extends Parser.From(Parser.mathTy) {
  lazy val Top: MathTy = MathTopTy
  lazy val Bot: MathTy = MathSetTy(Set.empty)
  lazy val Int: MathTy = MathIntTy(IntTy.Top)
  lazy val NonPosInt: MathTy = MathIntTy(IntTy.NonPos)
  lazy val NonNegInt: MathTy = MathIntTy(IntTy.NonNeg)
  lazy val NegInt: MathTy = MathIntTy(IntTy.Neg)
  lazy val PosInt: MathTy = MathIntTy(IntTy.Pos)
  lazy val Zero: MathTy = MathIntTy(IntTy.Zero)
  lazy val One: MathTy = MathIntTy(IntTy.One)
}
