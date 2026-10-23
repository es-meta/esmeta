package esmeta.ty

import esmeta.interpreter.*
import esmeta.state.{Math, Number}

/** arithmetic test for the numeric domains
  *
  * Checks each abstract operation against the concrete one in
  * `Interpreter.scala`, over the universes and witnesses held in `TyTest`.
  */
class ArithTinyTest extends TyTest {
  val name: String = "tyArithTest"

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
    checkArith()

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
