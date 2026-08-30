package esmeta.analyzer

import esmeta.interpreter.Interpreter
import esmeta.ir.{UOp, COp}
import esmeta.state.*
import esmeta.ty.*

/** transfer function test
  *
  * The lattice operations are covered by the `ty` tests; this checks that the
  * unary transfer functions over-approximate what the interpreter computes.
  */
class TransferSmallTest extends AnalyzerTest {
  val name: String = "analyzerTransferTest"

  private val checker = AnalyzerTest.tychecker
  import checker.{AbsValue, AbsState}
  private given AbsState = AbsState.Empty

  /** concrete witnesses, including the boundary values that the numeric domains
    * have had bugs on
    */
  private val maths = List[BigDecimal](-3, -2.5, -1, -0.5, 0, 0.5, 1, 2.5, 3)
    .map(Math(_))
  private val numbers = List(
    Double.NegativeInfinity,
    -2.5,
    -1.0,
    -0.0,
    0.0,
    1.0,
    2.5,
    Double.PositiveInfinity,
    Double.NaN,
  ).map(Number(_))
  private val values: List[Value] = maths ++ numbers ++ List(
    Infinity(true),
    Infinity(false),
    BigInt(7),
  )

  /** the type of a single concrete value, so that a witness can be lifted into
    * the abstract domain without going through the analyzer
    */
  private def tyOf(v: Value): ValueTy = v match
    case m: Math     => MathT(m.decimal)
    case n: Number   => NumberT(n)
    case Infinity(p) => if (p) InfinityT(true) else InfinityT(false)
    case BigInt(_)   => BigIntT
    case _           => AnyT

  private val tys: List[ValueTy] = List(
    MathT,
    ValueTy(math = MathTy.Int),
    ValueTy(math = MathTy.Pos),
    ValueTy(math = MathTy.Neg),
    ValueTy(math = MathTy.NonNeg),
    NumberT,
    ValueTy(number = NumberTy.Int),
    ValueTy(number = NumberTy.Pos),
    ValueTy(number = NumberTy.Neg),
    ValueTy(number = NumberTy.NaN),
    ValueTy(number = NumberTy.Infinite),
    InfinityT,
    BigIntT,
  ) ++ values.map(tyOf)

  /** Check that an abstract unary operation holds every value the interpreter
    * can produce from a value the operand holds.
    */
  private def checkUnary(
    desc: String,
    abs: AbsValue => AbsValue,
    con: Value => Option[Value],
  ): Unit =
    val violations = for {
      ty <- tys
      res = abs(AbsValue(ty)).ty
      v <- values if ty.contains(v, Heap())
      w <- con(v)
      if !res.contains(w, Heap())
    } yield s"$v -> $w is missing from $desc($ty) = $res"
    check(desc) {
      if (violations.nonEmpty) {
        println(s"[FAILED] $desc: ${violations.size} violation(s)")
        violations.distinct.take(10).foreach(v => println(s"- $v"))
        assert(violations.isEmpty)
      }
    }

  private def opt(f: => Value): Option[Value] =
    try Some(f)
    catch case _: Throwable => None

  // registration
  def init: Unit = {
    checkUnary("negation", -_, v => opt(Interpreter.eval(UOp.Neg, v)))
    checkUnary("bitwise negation", ~_, v => opt(Interpreter.eval(UOp.BNot, v)))
    checkUnary(
      "absolute",
      _.abs,
      { case m: Math => Some(Interpreter.abs(m)); case _ => None },
    )
    checkUnary(
      "floor",
      _.floor,
      { case m: Math => Some(Interpreter.floor(m)); case _ => None },
    )
  }

  init
}
