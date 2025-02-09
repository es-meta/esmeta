package esmeta.ty

import esmeta.interpreter.Interpreter
import esmeta.state.Math
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.util.domain.{*, given}, BSet.*, Flat.*

/** mathematical value types */
sealed trait MathTy extends TyElem {
  import MathTy.*

  /** top check */
  def isTop: Boolean = this.canon == Top

  /** bottom check */
  def isBottom: Boolean = this.canon match
    case MathSignTy(sign) => sign.isBottom
    case MathSetTy(set)   => set.isEmpty
    case MathIntTy(int)   => int.isBottom

  /** partial order */
  def <=(that: MathTy): Boolean = (this.canon, that.canon) match
    case _ if (this eq that) || (this == Bot) => true
    // same types
    case (MathSignTy(l), MathSignTy(r))     => l <= r
    case (MathIntTy(l), MathIntTy(r))       => l <= r
    case (MathSetTy(lset), MathSetTy(rset)) => lset subsetOf rset
    // subset
    case (MathIntTy(l), MathSignTy(r)) => l.toSign <= r
    // comparsion with set
    case (MathIntTy(int), MathSetTy(set)) =>
      int.toMathSet.fold(false) {
        case mset => mset subsetOf set
      }
    case (MathSetTy(set), MathIntTy(int))     => false
    case (l @ MathSetTy(lset), MathSignTy(r)) => l.toSign <= r
    case _                                    => false

  /** union type */
  def ||(that: MathTy): MathTy = (this.canon, that.canon) match
    case _ if this eq that            => this
    case (l, r) if l.isTop || r.isTop => Top
    // same types
    case (MathSignTy(l), MathSignTy(r))     => MathSignTy(l || r)
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l || r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset union rset)
    // comparison with set
    case (MathIntTy(int), MathSetTy(set)) => integrate(int, set)(_ union _)
    case (MathSetTy(set), MathIntTy(int)) => integrate(int, set)(_ union _)
    case (MathSignTy(sign), MathSetTy(set)) =>
      MathSetTy(set ++ set.filter(n => sign.contains(n.decimal)))
    case (MathSetTy(set), MathSignTy(sign)) =>
      MathSetTy(set ++ set.filter(n => sign.contains(n.decimal)))
    case (l, r) => MathSignTy(l.toSign || r.toSign)

  /** intersection type */
  def &&(that: MathTy): MathTy = (this.canon, that.canon) match
    case _ if this eq that                  => this
    case (l, r) if l.isBottom || r.isBottom => Bot
    // same types
    case (MathSignTy(l), MathSignTy(r))     => MathSignTy(l && r)
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l && r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset intersect rset)
    // comparison with set
    case (MathIntTy(int), MathSetTy(set)) => integrate(int, set)(_ intersect _)
    case (MathSetTy(set), MathIntTy(int)) => integrate(int, set)(_ intersect _)
    case (MathSignTy(sign), MathSetTy(set)) =>
      MathSetTy(set.filter(n => sign.contains(n.decimal)))
    case (MathSetTy(set), MathSignTy(sign)) =>
      MathSetTy(set.filter(n => sign.contains(n.decimal)))
    case (l, r) => MathSignTy(l.toSign && r.toSign)

  /** prune type */
  def --(that: MathTy): MathTy = (this.canon, that.canon) match
    case _ if this eq that               => Bot
    case (l, r) if l.isBottom || r.isTop => Bot
    // same types
    case (MathSignTy(l), MathSignTy(r))     => MathSignTy(l -- r)
    case (MathIntTy(l), MathIntTy(r))       => MathIntTy(l -- r)
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset -- rset)
    // comparison with set
    case (MathIntTy(int), MathSetTy(set)) => integrate(int, set)(_ -- _)
    case (MathSetTy(set), MathIntTy(int)) => integrate(int, set)(_ -- _)
    case (MathSetTy(set), MathSignTy(sign)) =>
      MathSetTy(set.filter(n => sign.contains(n.decimal)))
    case (l, r) => MathSignTy(l.toSign -- r.toSign)

  /** addition */
  def +(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l + r)
    case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l + r)
    case _                              => Top

  /** subtraction */
  def -(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l - r)
    case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l - r)
    case _                              => Top

  /** multiplcation */
  def *(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l * r)
    case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l * r)
    case _                              => Top

  /** modulo */
  def %(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l % r)
    case _                            => Top

  /** exponentiation */
  def **(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l ** r)
    case _                            => Top

  /** bitwise operation (&) */
  def &(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l & r)
    case _                            => Top

  /** bitwise operation (|) */
  def |(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l | r)
    case _                            => Top

  /** bitwise operation (^) */
  def ^(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l ^ r)
    case _                            => Top

  /** shift left */
  def <<(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l << r)
    case _                            => Top

  /** shift right */
  def >>(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l >> r)
    case _                            => Top

  /** unsigned shift right */
  def >>>(that: MathTy): MathTy = (this.canon, that.canon) match
    case (MathIntTy(l), MathIntTy(r)) => MathIntTy(l >>> r)
    case _                            => Top

  /** min operation */
  def min(that: MathTy): MathTy =
    import Math.given
    (this.canon, that.canon) match
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
    (this.canon, that.canon) match
      case (l, r) if l.isTop || r.isTop   => Top
      case (MathSignTy(l), MathSignTy(r)) => MathSignTy(l max r)
      case (MathIntTy(l), MathIntTy(r))   => MathIntTy(l max r)
      case (MathSetTy(lset), MathSetTy(rset)) =>
        if lset.min > rset.max then MathSetTy(lset)
        else if rset.min > lset.max then MathSetTy(rset)
        else Top
      case _ => Top

  /** inclusion check */
  def contains(math: Math): Boolean =
    import scala.math.BigInt
    this.canon match
      case MathSignTy(sign) => sign.contains(math.decimal)
      case MathIntTy(int) =>
        math.decimal.isWhole && int.contains(math.decimal.toBigInt)
      case MathSetTy(set) => set.contains(math)

  def contains(bint: esmeta.state.BigInt): Boolean = this.canon match
    case MathSignTy(sign) => sign.contains(bint.bigInt)
    case MathIntTy(int)   => int.contains(bint.bigInt)
    case MathSetTy(set)   => set.exists(_.decimal == bint.bigInt)

  /** flatten */
  def toFlat: Flat[Math] = this.canon match
    case MathSignTy(sign) => if sign.isZero then Flat(Math(0)) else Many
    case MathIntTy(int)   => int.toMathSet.fold(Flat.Zero)(Flat(_))
    case MathSetTy(set)   => Flat(set)

  /** integral check */
  def isInt: Boolean = this.canon match
    case MathIntTy(_) => true
    case _            => false

  /** non-positive check */
  def isNonPos: Boolean = this.canon match
    case MathSignTy(sign) => sign.isNonPos
    case MathIntTy(int)   => int.isNonPos
    case s                => s.toSignTy.isNonPos

  /** non-negative check */
  def isNonNeg: Boolean = this.canon match
    case MathSignTy(sign) => sign.isNonNeg
    case MathIntTy(int)   => int.isNonNeg
    case s                => s.toSignTy.isNonNeg

  /** negative check */
  def isNeg: Boolean = this.canon match
    case MathSignTy(sign) => sign.isNeg
    case MathIntTy(int)   => int.isNeg
    case s                => s.toSignTy.isNeg

  /** positive check */
  def isPos: Boolean = this.canon match
    case MathSignTy(sign) => sign.isPos
    case MathIntTy(int)   => int.isPos
    case s                => s.toSignTy.isPos

  /** non-positive integral check */
  def isNonPosInt: Boolean = this.canon match
    case MathIntTy(int) => int.isNonPos
    case _              => false

  /** non-negative integral check */
  def isNonNegInt: Boolean = this.canon match
    case MathIntTy(int) => int.isNonNeg
    case _              => false

  /** negative integral check */
  def isNegInt: Boolean = this.canon match
    case MathIntTy(int) => int.isNeg
    case _              => false

  /** positive integral check */
  def isPosInt: Boolean = this.canon match
    case MathIntTy(int) => int.isPos
    case _              => false

  /** to list of atomic math types */
  def toAtomicTys: List[MathTy] = this.canon match
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

  /** This returns an canonical form of the mathematical type. 1) If the type is
    * a set of integers, this must return MathIntTy. 2) Else if the gamma(type)
    * is finite, then this must return MathSetTy. 3) Otherwise, this must return
    * MathSignTy.
    *
    * @return
    *   canonical form of the mathematical type
    */
  def canon: MathTy = this match
    case MathSignTy(sign) =>
      if sign.isZero then MathIntTy(IntTy.Zero)
      else if sign.isBottom then MathIntTy(IntTy.Bot)
      else this
    case MathSetTy(set) =>
      if (set.forall(_.decimal.isWhole))
        MathIntTy(set.map(x => x.decimal.toBigInt))
      else this
    case i @ MathIntTy(_) => i

  def toSign: Sign = this.canon match
    case MathSignTy(sign) => sign
    case MathSetTy(set) =>
      Sign.alpha(
        set,
        v =>
          if v.decimal < 0 then Sign.Neg
          else if v.decimal > 0 then Sign.Pos
          else Sign.Zero,
      )
    case MathIntTy(int) => int.toSign

  /** This returns a sign type of the mathematical type. May not return a
    * canonical form.
    *
    * @return
    *   sign type of the mathematical type
    */
  def toSignTy: MathSignTy = this.canon match
    case s @ MathSignTy(sign) => s
    case MathSetTy(set)       => MathSignTy(this.toSign)
    case MathIntTy(int)       => MathSignTy(int.toSign)
}

/** mathematical value types */
case class MathSignTy(sign: Sign) extends MathTy

/** types for set of mathematical values */
case class MathSetTy(set: Set[Math]) extends MathTy

/** types for mathematical integers */
case class MathIntTy(int: IntTy) extends MathTy

object MathSetTy:
  def apply(seq: Math*): MathSetTy = MathSetTy(seq.toSet)

object MathIntTy:
  def apply(ints: Iterable[BigInt]): MathIntTy = MathIntTy(IntSetTy(ints.toSet))

object MathTy extends Parser.From(Parser.mathTy) {
  // Top & Bot
  lazy val Top: MathTy = MathSignTy(Sign.Top)
  lazy val Bot: MathTy = MathSignTy(Sign.Bot).canon

  // Signs
  lazy val NonPos: MathTy = MathSignTy(Sign.NonPos)
  lazy val NonNeg: MathTy = MathSignTy(Sign.NonNeg)
  lazy val Neg: MathTy = MathSignTy(Sign.Neg)
  lazy val Pos: MathTy = MathSignTy(Sign.Pos)

  // Integers
  lazy val Int: MathTy = MathIntTy(IntTy.Top)
  lazy val NonPosInt: MathTy = MathIntTy(IntTy.NonPos)
  lazy val NonNegInt: MathTy = MathIntTy(IntTy.NonNeg)
  lazy val NegInt: MathTy = MathIntTy(IntTy.Neg)
  lazy val PosInt: MathTy = MathIntTy(IntTy.Pos)

  // Constants
  lazy val Zero: MathTy = MathIntTy(IntTy.Zero)
  lazy val One: MathTy = MathIntTy(IntTy.One)

  /** This helper is for applying f between the given set and the given integer
    * domain. If the integer domain is not finite, this returns Top.
    *
    * @param int
    *   integer domain
    * @param set
    *   set of mathematical values
    * @param f
    *   function to apply between the given set and the given integer domain
    * @return
    *   result of applying f between the given set and the given integer domain,
    *   or Top if fails
    */
  private def integrate(int: IntTy, set: Set[Math])(
    f: (Set[Math], Set[Math]) => Set[Math],
  ): MathTy =
    int.toMathSet.fold(Top) {
      case mset => MathSetTy(f(mset, set))
    }
}
