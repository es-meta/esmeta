package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** number types
  *
  * A Number is either a finite value, an infinity, or *NaN*, and the three are
  * tracked apart so that one can never be mistaken for another. In particular
  * *+∞*<sub>𝔽</sub> is a Number but not a finite one, so it is never covered
  * by a sign of the finite part.
  */
case class NumberTy(finite: FinNumberTy, inf: InfinityTy, nan: Boolean)
  extends TyElem
  with Lattice[NumberTy] {
  import NumberTy.*

  /** top check */
  def isTop: Boolean = this.canon == Top

  /** bottom check */
  def isBottom: Boolean = finite.isBottom && inf.isBottom && !nan

  /** partial order/subset operator */
  def <=(that: => NumberTy): Boolean =
    (this.finite <= that.finite) && (this.inf <= that.inf) &&
    (!this.nan || that.nan)

  /** union type */
  def ||(that: => NumberTy): NumberTy =
    NumberTy(finite || that.finite, inf || that.inf, nan || that.nan)

  /** intersection type */
  def &&(that: => NumberTy): NumberTy =
    NumberTy(finite && that.finite, inf && that.inf, nan && that.nan)

  /** prune type */
  def --(that: => NumberTy): NumberTy =
    NumberTy(finite -- that.finite, inf -- that.inf, nan && !that.nan)

  /** inclusion check */
  def contains(number: Number): Boolean =
    if (number.isNaN) nan
    else if (number.double.isInfinite) inf.contains(number.double > 0)
    else finite.contains(number)

  /** NaN check */
  def hasNaN: Boolean = nan

  /** exactly the integral values of the type */
  def toIntTy: IntTy = finite.toIntTy

  /** get single value */
  def getSingle: Flat[Number] =
    val specials =
      (if (nan) List(Number(Double.NaN)) else Nil) ++
      inf.pos.toList.map(p =>
        Number(if (p) Double.PositiveInfinity else Double.NegativeInfinity),
      )
    if (specials.isEmpty) finite.getSingle
    else if (specials.sizeIs > 1 || !finite.isBottom) Many
    else Flat(specials.head)

  /** addition */
  def +(that: NumberTy): NumberTy = arith(that, _ + _, _ + _)

  /** subtraction */
  def -(that: NumberTy): NumberTy = arith(that, _ - _, _ - _)

  /** multiplication */
  def *(that: NumberTy): NumberTy = arith(that, _ * _, _ * _, true)

  /** the sign of the type, counting its infinities */
  private def extSign: Sign =
    finite.canon.toSign || Sign(inf.contains(false), false, inf.contains(true))

  /** division, which does not preserve integrality */
  def /(that: NumberTy): NumberTy =
    if (this.isBottom || that.isBottom) Bot
    else
      val l = this.finite.canon
      val r = that.finite.canon
      val lSign = l.toSign
      val rSign = r.toSign
      val lInf = !this.inf.isBottom
      val rInf = !that.inf.isBottom
      val resNaN =
        this.nan || that.nan || (lInf && rInf) || (lSign.zero && rSign.zero)
      // a nonzero integral divisor never makes the quotient larger
      val mayOverflow = r match
        case FinNumberIntTy(_) => rSign.zero
        case _                 => !r.isBottom
      val nonZeroDividend = lSign.neg || lSign.pos || lInf
      val resInf =
        // a zero divisor gives both infinities, since *+0* and *-0* are one sign
        if (rSign.zero && nonZeroDividend) InfinityTy.Top
        else if (lInf || (!l.isBottom && mayOverflow))
          NumberTy.infOfSign(this.extSign / that.extSign)
        else InfinityTy.Bot
      // a dividend over an infinity, and a quotient that underflows, give zero
      val quotient =
        if (!l.isBottom && (rInf || !r.isBottom)) (lSign / rSign) || Sign.Zero
        else lSign / rSign
      NumberTy(FinNumberSignTy(quotient), resInf, resNaN)

  /** An infinite operand makes an infinity possible, an indeterminate form such
    * as `inf - inf` gives *NaN*, and two finite operands can still overflow.
    */
  private def arith(
    that: NumberTy,
    intOp: (IntTy, IntTy) => IntTy,
    signOp: (Sign, Sign) => Sign,
    zeroTimesInf: Boolean = false,
  ): NumberTy =
    if (this.isBottom || that.isBottom) Bot
    else
      val lSign = this.finite.canon.toSign
      val rSign = that.finite.canon.toSign
      // an infinity survives a multiplication only against a nonzero factor
      val fromInf =
        if (!zeroTimesInf) !this.inf.isBottom || !that.inf.isBottom
        else
          (!this.inf.isBottom && (!that.inf.isBottom || rSign.neg || rSign.pos)) ||
          (!that.inf.isBottom && (!this.inf.isBottom || lSign.neg || lSign.pos))
      val resNaN =
        nan || that.nan || (!this.inf.isBottom && !that.inf.isBottom) ||
        (zeroTimesInf && ((!this.inf.isBottom && rSign.zero) ||
        (!that.inf.isBottom && lSign.zero)))
      lazy val resInf = NumberTy.infOfSign(signOp(this.extSign, that.extSign))
      def infOf(overflow: Boolean): InfinityTy =
        if (fromInf || overflow) resInf else InfinityTy.Bot
      (this.finite.canon, that.finite.canon) match
        // a double represents an integer exactly only up to 2^53
        case (FinNumberIntTy(l), FinNumberIntTy(r)) if exactInts(intOp(l, r)) =>
          NumberTy(FinNumberIntTy(intOp(l, r)), infOf(false), resNaN)
        case (l, r) =>
          val bothFin = !l.isBottom && !r.isBottom
          // a product of two finite values can underflow to a zero
          val sign = signOp(l.toSign, r.toSign)
          NumberTy(
            FinNumberSignTy(
              if (zeroTimesInf && bothFin) sign || Sign.Zero
              else sign,
            ),
            infOf(bothFin),
            resNaN,
          )

  /** non-negative integral check */
  def isNonNegInt: Boolean = isInt && finite.toIntTy.isNonNeg

  /** positive integral check */
  def isPosInt: Boolean = isInt && finite.toIntTy.isPos

  /** non-positive integral check */
  def isNonPosInt: Boolean = isInt && finite.toIntTy.isNonPos

  /** negative integral check */
  def isNegInt: Boolean = isInt && finite.toIntTy.isNeg

  private def isInt: Boolean = !nan && inf.isBottom && finite.isInt

  /** canonical form, which normalizes only the finite part */
  def canon: NumberTy = NumberTy(finite.canon, inf, nan)

  /** the sign of the finite part, ignoring infinities and *NaN* */
  def toSign: Sign = finite.toSign
}

/** finite number types */
sealed trait FinNumberTy extends TyElem with Lattice[FinNumberTy] {
  import FinNumberTy.*

  /** top check */
  def isTop: Boolean = this.canon == Top

  /** bottom check */
  def isBottom: Boolean = this.canon match
    case FinNumberSetTy(set)   => set.isEmpty
    case FinNumberIntTy(int)   => int.isBottom
    case FinNumberSignTy(sign) => sign.isBottom

  /** partial order/subset operator */
  def <=(that: => FinNumberTy): Boolean = (this.canon, that.canon) match
    case (l, r) if (l == r) || l.isBottom || r.isTop => true
    // a finite left-hand side is checked element-wise
    case (FinNumberSetTy(lset), r) => lset.forall(r.contains)
    // an integral left-hand side is covered by any superset of its integers
    case (FinNumberIntTy(l), FinNumberIntTy(r))  => l <= r
    case (FinNumberIntTy(l), FinNumberSignTy(r)) => l.toSign <= r
    case (FinNumberIntTy(l), FinNumberSetTy(rset)) =>
      l.toNumberSet.fold(false)(_ subsetOf rset)
    // only a sign type covers a whole sign class
    case (FinNumberSignTy(l), FinNumberSignTy(r)) => l <= r
    case (FinNumberSignTy(l), _)                  => l.isBottom

  /** union type */
  def ||(that: => FinNumberTy): FinNumberTy = (this.canon, that.canon) match
    case (l, r) if l.isTop || r.isTop           => Top
    case (l, r) if l.isBottom                   => r
    case (l, r) if r.isBottom                   => l
    case (FinNumberSetTy(l), FinNumberSetTy(r)) => FinNumberSetTy(l union r)
    case (FinNumberIntTy(l), FinNumberIntTy(r)) => FinNumberIntTy(l || r)
    // an integral domain joins a set only when it is finite itself
    case (FinNumberSetTy(l), FinNumberIntTy(r)) if r.toNumberSet.isDefined =>
      FinNumberSetTy(l ++ r.toNumberSet.get)
    case (FinNumberIntTy(l), FinNumberSetTy(r)) if l.toNumberSet.isDefined =>
      FinNumberSetTy(l.toNumberSet.get ++ r)
    // a set of integral values joins the integral domain, which keeps the
    // integrality at the cost of telling *+0* from *-0*
    case (FinNumberSetTy(l), FinNumberIntTy(r)) if l.forall(isIntegral) =>
      FinNumberIntTy(IntSetTy(l.flatMap(_.toBigIntExact)) || r)
    case (FinNumberIntTy(l), FinNumberSetTy(r)) if r.forall(isIntegral) =>
      FinNumberIntTy(l || IntSetTy(r.flatMap(_.toBigIntExact)))
    // otherwise widen both sides to signs
    case (l, r) => FinNumberSignTy(l.toSign || r.toSign)

  /** intersection type */
  def &&(that: => FinNumberTy): FinNumberTy = (this.canon, that.canon) match
    case (l, r) if l.isBottom || r.isBottom => Bot
    case (l, r) if l.isTop                  => r
    case (l, r) if r.isTop                  => l
    // a finite side bounds the result, so filter it element-wise
    case (FinNumberSetTy(lset), r) => FinNumberSetTy(lset.filter(r.contains))
    case (l, FinNumberSetTy(rset)) => FinNumberSetTy(rset.filter(l.contains))
    // otherwise meet the integral domains
    case (FinNumberIntTy(l), r) => FinNumberIntTy(l && r.toIntTy)
    case (FinNumberSignTy(l), FinNumberIntTy(r)) =>
      FinNumberIntTy(IntSignTy(l) && r)
    case (FinNumberSignTy(l), FinNumberSignTy(r)) => FinNumberSignTy(l && r)

  /** prune type */
  def --(that: => FinNumberTy): FinNumberTy = (this.canon, that.canon) match
    case (l, r) if l.isBottom || r.isTop => Bot
    // a finite left-hand side is pruned element-wise
    case (FinNumberSetTy(lset), r) => FinNumberSetTy(lset.filterNot(r.contains))
    // an integral left-hand side only loses the integers of the right side,
    // and its 0 stands for both signed zeros, so both must be covered
    case (FinNumberIntTy(l), r) =>
      val bothZeros = r.contains(Number(0.0)) && r.contains(Number(-0.0))
      FinNumberIntTy(
        l -- (if (bothZeros) r.toIntTy else r.toIntTy -- IntTy.Zero),
      )
    // both sides cover whole sign classes
    case (FinNumberSignTy(l), FinNumberSignTy(r)) => FinNumberSignTy(l -- r)
    // neither a set nor an integral domain covers a sign class of the reals,
    // so only the zero component can be ruled out
    case (FinNumberSignTy(l), r) =>
      val zero =
        l.zero && !(r.contains(Number(0.0)) && r.contains(Number(-0.0)))
      FinNumberSignTy(Sign(l.neg, zero, l.pos))

  /** inclusion check, for a finite number */
  def contains(number: Number): Boolean = this.canon match
    case FinNumberSignTy(sign) => sign.contains(number.double)
    case FinNumberIntTy(int)   => number.toBigIntExact.fold(false)(int.contains)
    case FinNumberSetTy(set)   => set contains number

  /** exactly the integral values of the type */
  def toIntTy: IntTy = this.canon match
    case FinNumberSignTy(sign) => IntSignTy(sign)
    case FinNumberIntTy(int)   => int
    case FinNumberSetTy(set)   => IntSetTy(set.flatMap(_.toBigIntExact))

  /** integral check */
  def isInt: Boolean = this.canon match
    case FinNumberIntTy(_) => true
    case _                 => false

  /** get single value */
  def getSingle: Flat[Number] = this.canon match
    case FinNumberSetTy(set) => Flat(set)
    case FinNumberIntTy(int) =>
      int.getSingle match
        // the integral number 0 stands for both *+0* and *-0*
        case One(x) if x == 0 => Many
        case flat             => flat.map(x => Number(x.toDouble))
    case FinNumberSignTy(sign) => if (sign.isBottom) Zero else Many

  /** This returns a canonical form of the finite number type. 1) If the type is
    * a set of integral numbers, this must return FinNumberIntTy. 2) Else if the
    * gamma(type) is finite, this must return FinNumberSetTy. 3) Otherwise, this
    * must return FinNumberSignTy.
    */
  def canon: FinNumberTy = this match
    case FinNumberSignTy(sign) if sign.isBottom => Bot
    case FinNumberSignTy(sign) if sign.isZero   => FinNumberIntTy(IntTy.Zero)
    case s @ FinNumberSignTy(_)                 => s
    case FinNumberIntTy(int)                    => FinNumberIntTy(int.canon)
    case s @ FinNumberSetTy(set) =>
      val ints = set.map(_.toBigIntExact)
      if (ints.forall(_.isDefined) && !set.exists(_.double == 0))
        FinNumberIntTy(IntSetTy(ints.flatten))
      else s

  /** sign of the type */
  def toSign: Sign = this.canon match
    case FinNumberSignTy(sign) => sign
    case FinNumberIntTy(int)   => int.toSign
    case FinNumberSetTy(set) =>
      Sign.alpha(
        set.map(_.double),
        x =>
          if (x < 0) Sign.Neg
          else if (x > 0) Sign.Pos
          else Sign.Zero,
      )
}

/** types for set of finite numbers */
case class FinNumberSetTy(set: Set[Number]) extends FinNumberTy

/** integral number types */
case class FinNumberIntTy(int: IntTy) extends FinNumberTy

/** finite number sign domain */
case class FinNumberSignTy(sign: Sign) extends FinNumberTy

object FinNumberTy {
  private def isIntegral(n: Number): Boolean = n.toBigIntExact.isDefined

  lazy val Top: FinNumberTy = FinNumberSignTy(Sign.Top)
  lazy val Bot: FinNumberTy = FinNumberIntTy(IntTy.Bot)

  extension (n: Number) {

    /** the exact integral value of the number, if any */
    def toBigIntExact: Option[scala.math.BigInt] =
      if (n.isNaN || n.double.isInfinite) None
      else scala.math.BigDecimal(n.double).toBigIntExact
  }
}

object NumberTy extends Parser.From(Parser.numberTy) {

  def exactInts(ty: IntTy): Boolean = ty.canon match
    case IntSetTy(set) => set.forall(_.abs <= maxExactInt)
    case _             => false

  def infOfSign(sign: Sign): InfinityTy = InfinityTy(
    (if (sign.pos) Set(true) else Set()) ++ (if (sign.neg) Set(false)
                                             else Set()),
  )

  lazy val maxExactInt: scala.math.BigInt = scala.math.BigInt(2).pow(53)

  /** Constants do not include NaN as default except Top. An infinity is a
    * Number, so a type described by a sign includes the infinity of that sign,
    * while an integral Number is finite by definition.
    */

  // one axis at a time
  lazy val Finite: NumberTy = finite(FinNumberTy.Top)
  lazy val PosInf: NumberTy = inf(InfinityTy.Pos)
  lazy val NegInf: NumberTy = inf(InfinityTy.Neg)
  lazy val Infinite: NumberTy = inf(InfinityTy.Top)
  lazy val NaN: NumberTy = NumberTy(FinNumberTy.Bot, InfinityTy.Bot, true)

  // Top & Bot
  lazy val Top: NumberTy = NumberTy(FinNumberTy.Top, InfinityTy.Top, true)
  lazy val Bot: NumberTy = NumberTy(FinNumberTy.Bot, InfinityTy.Bot, false)

  // Signs
  lazy val Pos: NumberTy =
    NumberTy(FinNumberSignTy(Sign.Pos), InfinityTy.Pos, false)
  lazy val Neg: NumberTy =
    NumberTy(FinNumberSignTy(Sign.Neg), InfinityTy.Neg, false)
  lazy val NonNeg: NumberTy =
    NumberTy(FinNumberSignTy(Sign.NonNeg), InfinityTy.Pos, false)
  lazy val NonPos: NumberTy =
    NumberTy(FinNumberSignTy(Sign.NonPos), InfinityTy.Neg, false)
  lazy val NonZero: NumberTy =
    NumberTy(FinNumberSignTy(Sign.NonZero), InfinityTy.Top, true)

  // Integers
  lazy val Int: NumberTy = int(IntTy.Top)
  lazy val NonPosInt: NumberTy = int(IntTy.NonPos)
  lazy val NonNegInt: NumberTy = int(IntTy.NonNeg)
  lazy val NegInt: NumberTy = int(IntTy.Neg)
  lazy val PosInt: NumberTy = int(IntTy.Pos)

  // Constants
  lazy val Zero: NumberTy = int(IntTy.Zero)
  lazy val One: NumberTy = int(IntTy.One)

  /** a type of finite Numbers */
  def finite(finite: FinNumberTy): NumberTy =
    NumberTy(finite, InfinityTy.Bot, false)

  /** a type of infinities alone */
  def inf(inf: InfinityTy): NumberTy = NumberTy(FinNumberTy.Bot, inf, false)

  /** a type of integral Numbers, which are finite by definition */
  def int(int: IntTy): NumberTy = finite(FinNumberIntTy(int))

  /** split a set of numbers into its finite, infinite, and NaN parts */
  def apply(set: Set[Number]): NumberTy = NumberTy(
    FinNumberSetTy(set.filter(n => !n.isNaN && !n.double.isInfinite)),
    InfinityTy(set.collect { case n if n.double.isInfinite => n.double > 0 }),
    set.exists(_.isNaN),
  ).canon

  extension (x: Set[Number]) {
    def hasNaN: Boolean = x.exists(_.isNaN)
  }
}
