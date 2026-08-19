package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** number types */
sealed trait NumberTy extends TyElem with Lattice[NumberTy] {
  import NumberTy.*

  /** top check */
  def isTop: Boolean = this.canon == Top

  /** bottom check */
  def isBottom: Boolean = this.canon == Bot

  /** partial order/subset operator */
  def <=(that: => NumberTy): Boolean = (this.canon, that.canon) match
    case _ if (this eq that) || (this == Bot) => true
    // same types
    case (NumberSignTy(lsign, lnan, lnz), NumberSignTy(rsign, rnan, rnz)) =>
      (lsign <= rsign) && (!lnan || rnan) && (!lnz || rnz)
    case (NumberIntTy(lint, lnan), NumberIntTy(rint, rnan)) =>
      (lint <= rint) && (!lnan || rnan)
    case (NumberSetTy(lset), NumberSetTy(rset)) => lset subsetOf rset
    // subset
    case (NumberIntTy(l, lnan), NumberSignTy(r, rnan, _)) =>
      l.toSignTy.sign <= r && (!lnan || rnan)
    // comparsion with set
    case (NumberIntTy(int, nan), s @ NumberSetTy(set)) =>
      int.toNumberSet.fold(false) {
        case mset => mset subsetOf set
      } && (!nan || s.hasNaN)
    case (l @ NumberSetTy(lset), NumberSignTy(r, nan, rnz)) =>
      l.toSignTy.sign <= r && (!l.hasNaN || nan) && (!l.hasNegZero || rnz)
    case (l @ NumberSetTy(lset), r) =>
      lset.forall(x => r.contains(x))
    case _ => false

  /** union type */
  def ||(that: => NumberTy): NumberTy = (this.canon, that.canon) match
    case _ if this eq that            => this
    case (l, r) if l.isTop || r.isTop => Top
    case (l, r) if l.isBottom         => r
    case (l, r) if r.isBottom         => l
    case (NumberSignTy(lsign, lnan, lnz), NumberSignTy(rsign, rnan, rnz)) =>
      NumberSignTy(lsign || rsign, lnan || rnan, lnz || rnz)
    case (NumberIntTy(lint, lnan), NumberIntTy(rint, rnan)) =>
      NumberIntTy(lint || rint, lnan || rnan)
    case (NumberSetTy(lset), NumberSetTy(rset)) =>
      NumberSetTy(lset union rset)
    case (NumberSetTy(lset), NumberIntTy(rint, rnan)) =>
      integrate(rint, lset, rnan)(_ union _, _ || _)
    case (NumberIntTy(lint, lnan), NumberSetTy(rset)) =>
      integrate(lint, rset, lnan)(_ union _, _ || _)
    case _ =>
      val thisSign = this.toSignTy
      val thatSign = that.toSignTy
      NumberSignTy(
        thisSign.sign || thatSign.sign,
        thisSign.hasNaN || thatSign.hasNaN,
        thisSign.negZero || thatSign.negZero,
      )

  /** intersection type */
  def &&(that: => NumberTy): NumberTy = (this.canon, that.canon) match
    case _ if this eq that                  => this
    case (l, r) if l.isBottom || r.isBottom => Bot
    case (l, r) if l.isTop                  => r
    case (l, r) if r.isTop                  => l
    case (NumberSignTy(lsign, lnan, lnz), NumberSignTy(rsign, rnan, rnz)) =>
      NumberSignTy(lsign && rsign, lnan && rnan, lnz && rnz)
    case (NumberIntTy(lint, lnan), NumberIntTy(rint, rnan)) =>
      NumberIntTy(lint && rint, lnan && rnan)
    case (NumberSetTy(lset), NumberSetTy(rset)) =>
      NumberSetTy(lset intersect rset)
    case (NumberSetTy(lset), NumberIntTy(rint, rnan)) =>
      integrate(rint, lset, rnan)(_ intersect _, _ && _)
    case (NumberIntTy(lint, lnan), NumberSetTy(rset)) =>
      integrate(lint, rset, lnan)(_ intersect _, _ && _)
    case (NumberSetTy(set), NumberSignTy(sign, nan, nz)) =>
      NumberSetTy(
        (if nan && set.hasNaN then Set(Number(Double.NaN))
         else Set()) ++
        set.filter(x =>
          if (isNegZero(x.double)) nz else sign.contains(x.double),
        ),
      )
    case (NumberSignTy(sign, nan, nz), NumberSetTy(set)) =>
      NumberSetTy(
        (if nan && set.hasNaN then Set(Number(Double.NaN))
         else Set()) ++
        set.filter(x =>
          if (isNegZero(x.double)) nz else sign.contains(x.double),
        ),
      )
    case _ =>
      val thisSign = this.toSignTy
      val thatSign = that.toSignTy
      NumberSignTy(
        thisSign.sign && thatSign.sign,
        thisSign.hasNaN && thatSign.hasNaN,
        thisSign.negZero && thatSign.negZero,
      )

  /** prune type */
  def --(that: => NumberTy): NumberTy =
    (this.canon, that.canon) match
      case _ if this eq that               => Bot
      case _ if this == Bot || that == Top => Bot
      case (NumberSignTy(lsign, lnan, lnz), NumberSignTy(rsign, rnan, rnz)) =>
        NumberSignTy(lsign -- rsign, lnan && !rnan, lnz && !rnz)
      case (NumberIntTy(lint, lnan), NumberIntTy(rint, rnan)) =>
        NumberIntTy(lint -- rint, lnan && !rnan)
      case (NumberSetTy(lset), NumberSetTy(rset)) =>
        NumberSetTy(lset -- rset)
      case (NumberIntTy(lint, lnan), NumberSetTy(rset)) =>
        val ints = rset.collect {
          case n if !n.double.isNaN && n.double.isWhole =>
            scala.math.BigInt(n.double.toLong)
        }
        NumberIntTy(
          lint -- IntSetTy(ints),
          lnan && !rset.exists(_.double.isNaN),
        )
      case (NumberSetTy(lset), NumberIntTy(rint, rnan)) =>
        NumberSetTy(lset.filter { n =>
          if (n.double.isNaN) !rnan
          else
            !(n.double.isWhole && rint.contains(
              scala.math.BigInt(n.double.toLong),
            ))
        })
      case (NumberSignTy(lsign, lnan, lnz), NumberIntTy(rint, rnan)) =>
        val zero = rint.canon match
          case IntSignTy(s)  => s.zero
          case IntSetTy(set) => set.exists(_ == 0)
        NumberSignTy(lsign -- Sign(false, zero, false), lnan && !rnan, lnz)
      case (NumberIntTy(lint, lnan), NumberSignTy(rsign, rnan, _)) =>
        NumberIntTy(lint -- IntSignTy(rsign), lnan && !rnan)
      case (NumberSignTy(lsign, lnan, lnz), r @ NumberSetTy(rset)) =>
        NumberSignTy(
          lsign -- Sign(
            false,
            rset.exists(x => x.double == 0.0 && !isNegZero(x.double)),
            false,
          ),
          lnan && !rset.exists(_.double.isNaN),
          lnz && !r.hasNegZero,
        )
      case (NumberSetTy(lset), NumberSignTy(rsign, rnan, rnz)) =>
        NumberSetTy(lset.filter { n =>
          if (n.double.isNaN) !rnan
          else if (isNegZero(n.double)) !rnz
          else !rsign.contains(n.double)
        })
      case _ =>
        val thisSign = this.toSignTy
        val thatSign = that.toSignTy
        NumberSignTy(
          thisSign.sign -- thatSign.sign,
          thisSign.hasNaN && !thatSign.hasNaN,
          thisSign.negZero && !thatSign.negZero,
        )

  /** inclusion check */
  def contains(number: Number): Boolean = this.canon match
    case NumberSignTy(sign, hasNaN, negZero) =>
      if number.isNaN then hasNaN
      else if isNegZero(number.double) then negZero
      else sign.contains(number.double)
    case NumberIntTy(int, hasNaN) =>
      if number.isNaN then hasNaN
      else number.double.isWhole && int.contains(number.double.toInt)
    case NumberSetTy(set) => set contains number

  /** get the finite set of numbers */
  def toNumberSet: Option[Set[Number]] = this.canon match
    case NumberSetTy(set) => Some(set)
    case NumberIntTy(int, nan) =>
      int.toNumberSet.map(set => if (nan) set + Number(Double.NaN) else set)
    case _ => None // None if the type is infinite

  /** get single value */
  def getSingle: Flat[Number] = this.canon match
    case s if s.isBottom  => esmeta.util.Zero
    case NumberSetTy(set) => Flat(set)
    case NumberIntTy(int, nan) =>
      if nan && int.isBottom then Flat(Number(Double.NaN))
      else int.getSingle.map(x => Number(x.toDouble))
    case NumberSignTy(sign, nan, nz) =>
      if nan && sign.isBottom && !nz then Flat(Number(Double.NaN))
      else if sign.isBottom && !nan && nz then Flat(Number(-0.0))
      else if sign.isZero && !nan && !nz then Flat(Number(0))
      else Many

  /** addition */
  def +(that: NumberTy): NumberTy =
    (this.canon, that.canon) match
      case (NumberIntTy(lint, lnan), NumberIntTy(rint, rnan)) =>
        NumberIntTy(lint + rint, lnan || rnan)
      case (l, r) =>
        val s = l.toSign + r.toSign
        NumberSignTy(s, l.toSignTy.hasNaN || r.toSignTy.hasNaN, s.zero)

  /** subtraction */
  def -(that: NumberTy): NumberTy = (this.canon, that.canon) match
    case (NumberIntTy(lint, lnan), NumberIntTy(rint, rnan)) =>
      NumberIntTy(lint - rint, lnan || rnan)
    case (l, r) =>
      val s = l.toSign - r.toSign
      NumberSignTy(s, l.toSignTy.hasNaN || r.toSignTy.hasNaN, s.zero)

  /** multiplication */
  def *(that: NumberTy): NumberTy = (this.canon, that.canon) match
    case (NumberIntTy(lint, lnan), NumberIntTy(rint, rnan)) =>
      NumberIntTy(lint * rint, lnan || rnan)
    case (l, r) =>
      val s = l.toSign * r.toSign
      NumberSignTy(s, l.toSignTy.hasNaN || r.toSignTy.hasNaN, s.zero)

  /** division */
  def /(that: NumberTy): NumberTy = (this.canon, that.canon) match
    case (NumberIntTy(lint, lnan), NumberIntTy(rint, rnan)) =>
      NumberIntTy(lint / rint, lnan || rnan)
    case (l, r) =>
      val s = l.toSign / r.toSign
      NumberSignTy(s, l.toSignTy.hasNaN || r.toSignTy.hasNaN, s.zero)

  /** non-negative integral check */
  def isNonNegInt: Boolean = this.canon match
    case NumberIntTy(int, false) => int.isNonNeg
    case _                       => false

  /** positive integral check */
  def isPosInt: Boolean = this.canon match
    case NumberIntTy(int, false) => int.isPos
    case _                       => false

  /** non-positive integral check */
  def isNonPosInt: Boolean = this.canon match
    case NumberIntTy(int, false) => int.isNonPos
    case _                       => false

  /** negative integral check */
  def isNegInt: Boolean = this.canon match
    case NumberIntTy(int, false) => int.isNeg
    case _                       => false

  /** This returns an canonical form of the number type. 1) If the type is a set
    * of integers, this must return NumberIntTy. 2) Else if the gamma(type) is
    * finite, then this must return NumberSetTy. 3) Otherwise, this must return
    * NumberSignTy.
    *
    * IMPORTANT: NaN value should be considered as NumberSetTy (Temporal).
    *
    * @return
    *   canonical form of the number type
    */
  def canon: NumberTy = this match
    case NumberSignTy(sign, hasNaN, negZero) =>
      // a shape holding only zeros is finite, so the set form is canonical
      lazy val nan = if (hasNaN) Set(Number(Double.NaN)) else Set[Number]()
      sign match
        case s if negZero && s.isBottom => NumberSetTy(Set(Number(-0.0)) ++ nan)
        case s if negZero && s.isZero =>
          NumberSetTy(Set(Number(-0.0), Number(0.0)) ++ nan)
        case _ if negZero       => this
        case s if sign.isZero   => NumberIntTy(IntTy.Zero, hasNaN)
        case s if sign.isBottom => if hasNaN then NumberTy.NaN else NumberTy.Bot
        case _                  => this
    case NumberIntTy(int, hasNaN) => NumberIntTy(int.canon, hasNaN)
    case s @ NumberSetTy(set) =>
      if (
        set.forall(x => (x.double.isWhole && !isNegZero(x.double)) || x.isNaN)
      )
        NumberIntTy(
          IntSetTy((set - Number.NaN).map(x => x.double.toLong)),
          set.hasNaN,
        )
      else this

  /** Get the sign of the number type. Ignores NaN values.
    *
    * @return
    *   the sign of the number type
    */
  def toSign: Sign = this.canon match
    case NumberSignTy(sign, _, nz) => Sign(sign.neg, sign.zero || nz, sign.pos)
    case NumberIntTy(int, _)       => int.toSign
    case NumberSetTy(set) =>
      Sign.alpha(
        set.map(_.double),
        x =>
          if x.isNaN then Sign.Bot
          else if x < 0 then Sign.Neg
          else if x > 0 then Sign.Pos
          else Sign.Zero,
      )

  /** Overapproximate a number type to a sign type. Also considers NaN values.
    *
    * @return
    *   the sign type
    */
  def toSignTy: NumberSignTy = this.canon match
    case s @ NumberSignTy(_, _, _) => s
    case NumberIntTy(int, hasNaN)  => NumberSignTy(int.toSign, hasNaN)
    case s @ NumberSetTy(set) =>
      val rest = set.filterNot(n => isNegZero(n.double))
      NumberSignTy(NumberSetTy(rest).toSign, s.hasNaN, s.hasNegZero)

}

/** number sign domain, where `sign.zero` covers +0 only */
case class NumberSignTy(sign: Sign, hasNaN: Boolean, negZero: Boolean = false)
  extends NumberTy

/** integral number types */
case class NumberIntTy(int: IntTy, hasNaN: Boolean) extends NumberTy

/** types for set of numbers */
case class NumberSetTy(set: Set[Number]) extends NumberTy {
  def hasNegZero: Boolean = set.exists(n => isNegZero(n.double))

  private def toInt: Option[IntSetTy] =
    val setWithoutNaN = set.filterNot(_.isNaN)
    if (setWithoutNaN.forall(_.double.isWhole))
      Some(IntSetTy(set.map(_.double.toInt)))
    else None

  def hasNaN: Boolean = set.exists(_.isNaN)
}

object NumberTy extends Parser.From(Parser.numberTy) {

  /** Constants do not includes NaN as default except Top
    */

  // Top & Bot
  lazy val Top: NumberTy = NumberSignTy(Sign.Top, true, true)
  lazy val Bot: NumberTy = NumberIntTy(IntTy.Bot, false)

  // Signs
  lazy val Pos: NumberTy = NumberSignTy(Sign.Pos, false)
  lazy val Neg: NumberTy = NumberSignTy(Sign.Neg, false, true)
  lazy val NonNeg: NumberTy = NumberSignTy(Sign.NonNeg, false, false)
  lazy val NonPos: NumberTy = NumberSignTy(Sign.NonPos, false, true)

  // Integers
  lazy val Int: NumberTy = NumberIntTy(IntTy.Top, false)
  lazy val NonPosInt: NumberTy = NumberIntTy(IntTy.NonPos, false)
  lazy val NonNegInt: NumberTy = NumberIntTy(IntTy.NonNeg, false)
  lazy val NegInt: NumberTy = NumberIntTy(IntTy.Neg, false)
  lazy val PosInt: NumberTy = NumberIntTy(IntTy.Pos, false)

  // Constants
  lazy val Zero: NumberTy = NumberIntTy(IntTy.Zero, false)
  lazy val One: NumberTy = NumberIntTy(IntTy.One, false)
  lazy val NaN: NumberTy = NumberSetTy(Set(Number(Double.NaN)))

  // abstraction of a concrete number value
  def apply(d: Double): NumberTy =
    if d.isNaN then NaN
    else if d.isWhole && !isNegZero(d) then NumberIntTy(IntTy(d.toInt), false)
    else NumberSetTy(Set(Number(d)))

  /** This helper is for applying f between the given set and the given integer
    * domain. If the integer domain is not finite, this returns Top.
    *
    * @param int
    *   integer domain
    * @param set
    *   set of number values
    * @param hasNan
    *   whether integer domain contains NaN
    * @param f
    *   function to apply between the given set and the given integer domain
    * @return
    *   result of applying f between the given set and the given integer domain,
    *   or Top if fails
    */
  private def integrate(int: IntTy, set: Set[Number], hasNan: Boolean)(
    f: (Set[Number], Set[Number]) => Set[Number],
    g: (Sign, Sign) => Sign,
  ): NumberTy =
    val NaN = Number(Double.NaN)
    int match
      case i @ IntSetTy(iset) =>
        val nset = iset.map(x => Number(x.toDouble))
        val s =
          if hasNan then nset + NaN
          else nset
        NumberSetTy(f(s, set)).canon
      case IntSignTy(lsign) =>
        val rsign = Sign.alpha(
          set.map(_.double),
          x =>
            if x.isNaN then Sign.Bot
            else if x < 0 then Sign.Neg
            else if x > 0 then Sign.Pos
            else Sign.Zero,
        )
        val n = {
          val l = if (hasNan) Set(NaN) else Set()
          val r = if (set.hasNaN) Set(NaN) else Set()
          f(l, r).nonEmpty
        }
        NumberIntTy(IntSignTy(g(lsign, rsign)), n).canon

  extension (x: Set[Number]) {
    def hasNaN: Boolean = x.exists(_.isNaN)
  }
}
