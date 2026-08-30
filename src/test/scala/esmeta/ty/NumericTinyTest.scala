package esmeta.ty

import esmeta.interpreter.*
import esmeta.state.{Math, Number}

/** numeric domain test */
class NumericTinyTest extends TyTest {
  val name: String = "tyNumericTest"

  private def numSet(ds: Double*) = NumberTy(ds.map(Number(_)).toSet)
  private def mathSet(ds: BigDecimal*) = MathSetTy(ds.map(Math(_)).toSet)
  private def intSet(xs: BigInt*) = IntSetTy(xs.toSet)

  private val huge = BigInt("100000000000000000000")

  /** concrete values used to witness the abstract operations */
  private val numbers = List(
    Double.NegativeInfinity,
    -3.0,
    -2.5,
    -1.0,
    -0.0,
    0.0,
    1.0,
    2.5,
    3.0,
    3.0e9, // beyond Int range
    9007199254740992.0, // 2^53, the last exactly represented integer
    Double.MinPositiveValue, // a product that underflows to a zero
    Double.MaxValue, // a sum that overflows to an infinity
    Double.PositiveInfinity,
    Double.NaN,
  ).map(Number(_))
  private val maths = List[BigDecimal](
    -3,
    -2.5,
    -1,
    -0.5,
    0,
    0.5,
    1,
    2.5,
    3,
    BigDecimal(huge),
  ).map(Math(_))
  private val bigInts = List[BigInt](-3, -1, 0, 1, 3, huge)

  /** every shape of every domain, including the redundant encodings of top and
    * bottom that `canon` is meant to collapse
    */
  private val numberTys: List[NumberTy] = List(
    NumberTy.Top,
    NumberTy.Bot,
    NumberTy.NaN,
    NumberTy.Infinite,
    NumberTy.Pos,
    NumberTy.Neg,
    NumberTy.NonNeg,
    NumberTy.NonPos,
    NumberTy.NonZero,
    NumberTy.Int,
    NumberTy.PosInt,
    NumberTy.NegInt,
    NumberTy.NonNegInt,
    NumberTy.NonPosInt,
    NumberTy.Zero,
    NumberTy.One,
    // redundant encodings
    NumberTy.Finite,
    NumberTy(FinNumberSignTy(Sign.Pos), InfinityTy.Bot, true),
    NumberTy.Bot,
    NumberTy.NaN,
    NumberTy.finite(FinNumberSignTy(Sign.Zero)),
    NumberTy(FinNumberIntTy(IntTy.Top), InfinityTy.Bot, true),
    NumberTy.NaN,
    NumberTy.int(IntSignTy(Sign.Bot)),
    numSet(),
    // a finite part alongside each special value
    NumberTy(FinNumberIntTy(IntTy.Top), InfinityTy.Top, false),
    NumberTy(FinNumberSignTy(Sign.Pos), InfinityTy.Neg, false),
    NumberTy(FinNumberSignTy(Sign.NonNeg), InfinityTy.Top, true),
    // signed zeros, NaN, and infinities
    numSet(0.0),
    numSet(-0.0),
    numSet(0.0, -0.0),
    numSet(Double.NaN),
    NumberTy.PosInf,
    NumberTy.NegInf,
    numSet(Double.PositiveInfinity, Double.NegativeInfinity, Double.NaN),
    numSet(Double.PositiveInfinity, 0.0, -0.0),
    // non-integral and out-of-Int-range values
    numSet(2.5),
    // the boundary of exact integer representation, and beyond it
    NumberTy.int(intSet(BigInt(2).pow(53))),
    numSet(Double.MaxValue),
    numSet(1.0, 2.5),
    numSet(Double.NaN, 2.5),
    numSet(3.0e9),
    NumberTy.int(intSet(3000000000L)),
    NumberTy.int(intSet(huge)),
  )
  private val mathTys: List[MathTy] = List(
    MathTy.Top,
    MathTy.Bot,
    MathTy.Pos,
    MathTy.Neg,
    MathTy.NonNeg,
    MathTy.NonPos,
    MathTy.Int,
    MathTy.PosInt,
    MathTy.NegInt,
    MathTy.NonNegInt,
    MathTy.NonPosInt,
    MathTy.Zero,
    MathTy.One,
    // redundant encodings
    MathSignTy(Sign.Bot),
    MathSignTy(Sign.Zero),
    MathIntTy(IntTy.Bot),
    MathIntTy(IntSignTy(Sign.Bot)),
    mathSet(),
    // sets mixing integral and non-integral values
    mathSet(0),
    mathSet(1),
    mathSet(0.5),
    mathSet(-0.5),
    mathSet(-0.5, 0.5),
    mathSet(1, 0.5),
    mathSet(-1, 0, 1),
    mathSet(BigDecimal(huge)),
    MathIntTy(intSet(-1, 1)),
    MathIntTy(intSet(huge)),
  )
  private val intTys: List[IntTy] = List(
    IntTy.Top,
    IntTy.Bot,
    IntTy.Pos,
    IntTy.Neg,
    IntTy.NonNeg,
    IntTy.NonPos,
    IntTy.Zero,
    IntTy.One,
    // redundant encodings
    IntSignTy(Sign.Bot),
    IntSignTy(Sign.Zero),
    intSet(),
    intSet(-1, 1),
    intSet(0, 3),
    intSet(huge),
    intSet(-huge, huge),
  )

  /** Check that each arithmetic operation over-approximates the concrete one
    * that the interpreter performs.
    */
  private def checkArith(): Unit =
    def opt[T](f: => T): Option[T] =
      try Some(f)
      catch case _: Throwable => None
    val violations =
      (for {
        (name, abs, con) <- List[
          (
            String,
            (IntTy, IntTy) => IntTy,
            (BigInt, BigInt) => Option[BigInt],
          ),
        ](
          ("+", _ + _, (x, y) => opt(x + y)),
          ("-", _ - _, (x, y) => opt(x - y)),
          ("*", _ * _, (x, y) => opt(x * y)),
          ("%", _ % _, (x, y) => if (y == 0) None else opt(x %% y)),
          (
            "**",
            _ ** _,
            (x, y) => if (y < 0 || y > 1024) None else opt(x.pow(y.toInt)),
          ),
          ("&", _ & _, (x, y) => opt(x & y)),
          ("|", _ | _, (x, y) => opt(x | y)),
          ("^", _ ^ _, (x, y) => opt(x ^ y)),
          (
            "<<",
            _ << _,
            (x, y) => if (y.abs > 1024) None else opt(x << y.toInt),
          ),
          (
            ">>",
            _ >> _,
            (x, y) => if (y.abs > 1024) None else opt(x >> y.toInt),
          ),
        )
        a <- intTys
        b <- intTys
        r = abs(a, b)
        x <- bigInts if a.contains(x)
        y <- bigInts if b.contains(y)
        v <- con(x, y)
        if !r.contains(v)
      } yield s"$x $name $y = $v is missing from ($a $name $b) = $r") ++
      (for {
        (name, abs, con) <- List[
          (
            String,
            (MathTy, MathTy) => MathTy,
            (BigDecimal, BigDecimal) => Option[BigDecimal],
          ),
        ](
          ("+", _ + _, (x, y) => opt(x + y)),
          ("-", _ - _, (x, y) => opt(x - y)),
          ("*", _ * _, (x, y) => opt(x * y)),
          ("%", _ % _, (x, y) => if (y == 0) None else opt(x %% y)),
          // the interpreter falls back to double arithmetic for a negative
          // exponent, which need not be an integer
          (
            "**",
            _ ** _,
            (x, y) => opt(BigDecimal(math.pow(x.toDouble, y.toDouble))),
          ),
          ("&", _ & _, (x, y) => opt(BigDecimal(x.toBigInt & y.toBigInt))),
          ("|", _ | _, (x, y) => opt(BigDecimal(x.toBigInt | y.toBigInt))),
          ("^", _ ^ _, (x, y) => opt(BigDecimal(x.toBigInt ^ y.toBigInt))),
          (
            "<<",
            _ << _,
            (x, y) =>
              if (y.abs > 1024) None
              else opt(BigDecimal(x.toBigInt << y.toInt)),
          ),
          (
            ">>",
            _ >> _,
            (x, y) =>
              if (y.abs > 1024) None
              else opt(BigDecimal(x.toBigInt >> y.toInt)),
          ),
        )
        a <- mathTys
        b <- mathTys
        r = abs(a, b)
        x <- maths if a.contains(x)
        y <- maths if b.contains(y)
        v <- con(x.decimal, y.decimal)
        if !r.contains(Math(v))
      } yield s"$x $name $y = $v is missing from ($a $name $b) = $r")
    val numberViolations = for {
      (name, abs, con) <- List[
        (String, (NumberTy, NumberTy) => NumberTy, (Double, Double) => Double),
      ](
        ("+", _ + _, _ + _),
        ("-", _ - _, _ - _),
        ("*", _ * _, _ * _),
        ("/", _ / _, _ / _),
      )
      a <- numberTys
      b <- numberTys
      r = abs(a, b)
      x <- numbers if a.contains(x)
      y <- numbers if b.contains(y)
      v = Number(con(x.double, y.double))
      if !r.contains(v)
    } yield s"$x $name $y = $v is missing from ($a $name $b) = $r"

    check("number arithmetic laws") {
      if (numberViolations.nonEmpty) {
        println(
          s"[FAILED] number arithmetic laws: ${numberViolations.size} violation(s)",
        )
        numberViolations.distinct.take(10).foreach(v => println(s"- $v"))
        assert(numberViolations.isEmpty)
      }
    }

    check("arithmetic laws") {
      if (violations.nonEmpty) {
        println(s"[FAILED] arithmetic laws: ${violations.size} violation(s)")
        violations.distinct.take(10).foreach(v => println(s"- $v"))
        assert(violations.isEmpty)
      }
    }

  // registration
  def init: Unit = {
    checkLaws("number lattice laws")(
      Domain[NumberTy, Number](
        numberTys,
        numbers,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.canon,
        _.isBottom,
      ),
    )
    checkLaws("math lattice laws")(
      Domain[MathTy, Math](
        mathTys,
        maths,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.canon,
        _.isBottom,
      ),
    )
    checkLaws("int lattice laws")(
      Domain[IntTy, BigInt](
        intTys,
        bigInts,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.canon,
        _.isBottom,
      ),
    )

    checkArith()

    checkEqual("canonicalize")(
      MathIntTy(intSet(1, 2)).canon -> MathIntTy(intSet(1, 2)),
      MathSignTy(Sign.Zero).canon -> MathIntTy(IntTy.Zero),
      mathSet(3, 5, 2).canon -> MathIntTy(intSet(3, 5, 2)),
      mathSet(0.5).canon -> mathSet(0.5),
      (NumberTy.NonNegInt || NumberTy.NaN) ->
      NumberTy(FinNumberIntTy(IntTy.NonNeg), InfinityTy.Bot, true),
    )

    // a finite set cannot rule out an infinite sign class
    checkEqual("prune by a finite set")(
      (IntTy.Top -- IntTy.One).contains(BigInt(2)) -> true,
      (MathTy.Top -- MathTy.One).contains(Math(2)) -> true,
      (MathTy.Top -- MathTy.One).contains(Math(0.5)) -> true,
      (NumberTy.Top -- numSet(Double.PositiveInfinity))
        .contains(Number(1.0)) -> true,
      // ... but the zero component is a single value, so it can be
      (IntTy.Top -- IntTy.Zero).contains(BigInt(0)) -> false,
      (MathTy.Top -- MathTy.Zero).contains(Math(0)) -> false,
    )

    // an integral domain cannot rule out the non-integral reals
    checkEqual("prune by an integral domain")(
      (NumberTy.Pos -- NumberTy.Int).contains(Number(2.5)) -> true,
      (MathTy.Top -- MathTy.Int).contains(Math(0.5)) -> true,
      // the sign domain cannot describe "non-integral", so only zero goes
      (MathTy.Top -- MathTy.Int).contains(Math(0)) -> false,
      (MathTy.Top -- MathTy.Int).contains(Math(1)) -> true,
    )

    // pruning must keep what is outside, not what is inside
    checkEqual("prune a set by a sign")(
      (mathSet(-0.5, 0.5) -- MathTy.Pos) -> mathSet(-0.5),
      (mathSet(-0.5, 0.5) -- MathTy.Neg) -> mathSet(0.5),
      (mathSet(-0.5, 0.5) -- MathTy.Top).isBottom -> true,
    )

    // a join may not drop either operand
    checkEqual("join across representations")(
      (MathTy.Neg || mathSet(0.5)).contains(Math(-1)) -> true,
      (MathTy.Neg || mathSet(0.5)).contains(Math(0.5)) -> true,
      (NumberTy.Int || numSet(Double.PositiveInfinity))
        .contains(Number(Double.PositiveInfinity)) -> true,
      (NumberTy.Zero || numSet(Double.PositiveInfinity))
        .contains(Number(-0.0)) -> true,
    )

    // membership must not be decided by a lossy conversion
    checkEqual("membership of large integral numbers")(
      NumberTy.int(intSet(Int.MaxValue)).contains(Number(3.0e9)) -> false,
      NumberTy.int(intSet(3000000000L)).contains(Number(3.0e9)) -> true,
      NumberTy.int(intSet(huge)).contains(Number(1.0)) -> false,
    )

    // the integral number 0 stands for both *+0* and *-0*
    checkEqual("signed zeros")(
      NumberTy.Zero.contains(Number(-0.0)) -> true,
      NumberTy.Zero.contains(Number(0.0)) -> true,
      IntTy.Zero.toNumberSet -> Some(Set(Number(0.0), Number(-0.0))),
    )

    // NaN is a value of its own, never reachable through a sign
    checkEqual("NaN")(
      NumberTy.Top.contains(Number(Double.NaN)) -> true,
      NumberTy.Pos.contains(Number(Double.NaN)) -> false,
      (NumberTy.Top -- NumberTy.NaN).contains(Number(Double.NaN)) -> false,
      (NumberTy.Top -- NumberTy.NaN).contains(Number(1.0)) -> true,
      numSet(Double.NaN).contains(Number(Double.NaN)) -> true,
    )

    // the modulo of an unbounded integer keeps the sign of the divisor
    checkEqual("modulo")(
      (IntTy.Top % intSet(2)) -> intSet(0, 1),
      (IntTy.Pos % intSet(2)) -> intSet(0, 1),
      (IntTy.Top % intSet(7)) -> intSet(0, 1, 2, 3, 4, 5, 6),
      (IntTy.Top % intSet(24)) -> IntTy.NonNeg,
      (IntTy.Top % intSet(-24)) -> IntTy.NonPos,
      (IntTy.Top % intSet(0)).isBottom -> true,
    )

    // a non-negative exponent keeps the sign of the base
    checkEqual("exponentiation")(
      (intSet(10) ** IntTy.NonNeg) -> IntTy.Pos,
      (intSet(10) ** intSet(2)) -> intSet(100),
      (IntTy.NonNeg ** IntTy.NonNeg) -> IntTy.NonNeg,
      (IntTy.Top ** IntTy.NonNeg) -> IntTy.Top,
      (intSet(10) ** IntTy.Neg) -> IntTy.Top,
    )
  }

  init
}
