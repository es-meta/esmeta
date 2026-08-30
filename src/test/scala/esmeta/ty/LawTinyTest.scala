package esmeta.ty

import esmeta.es.*
import esmeta.state.{BigInt as StateBigInt, *}
import esmeta.util.*

/** lattice law test for every type domain
  *
  * The universes and the `checkLaws` harness live in `TyTest`, so that this
  * file and `ArithTinyTest` check the same types.
  */
class LawTinyTest extends TyTest {
  val name: String = "tyLawTest"

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

    checkLaws("list lattice laws")(
      Domain[ListTy, Addr](
        listTys,
        listAddrs,
        (t, a) => t.contains(listObj(a), heap),
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.normalized,
        _.isBottom,
      ),
    )

    checkLaws("map lattice laws")(
      Domain[MapTy, Addr](
        mapTys,
        mapAddrs,
        (t, a) => t.contains(mapObj(a), heap),
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.normalized,
        _.isBottom,
      ),
    )

    checkLaws("ast lattice laws")(
      Domain[AstTy, AstValue](
        astTys,
        asts,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    checkLaws("bool lattice laws")(
      Domain[BoolTy, Boolean](
        boolTys,
        bools,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    checkLaws("bounded set lattice laws")(
      Domain[BSet[String], String](
        strTys,
        strs,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    checkLaws("record lattice laws")(
      Domain[RecordTy, Addr](
        recordTys,
        recordAddrs,
        (t, a) => t.contains(recordObj(a), heap),
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.normalized,
        _.isBottom,
      ),
    )

    checkLaws("value lattice laws")(
      Domain[ValueTy, Value](
        valueTys,
        valueWitnesses,
        (t, v) => t.contains(v, heap),
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    checkEqual("every component of `Any` is top")(
      AnyT.clo.isTop -> true,
      AnyT.cont.isTop -> true,
      AnyT.record.isTop -> true,
      AnyT.map.isTop -> true,
      AnyT.list.isTop -> true,
      AnyT.ast.isTop -> true,
      AnyT.grammarSymbol.isTop -> true,
      AnyT.codeUnit -> true,
      AnyT.enumv.isTop -> true,
      AnyT.math.isTop -> true,
      AnyT.infinity.isTop -> true,
      AnyT.number.isTop -> true,
      AnyT.bigInt -> true,
      AnyT.str.isTop -> true,
      AnyT.bool.isTop -> true,
      AnyT.undef -> true,
      AnyT.nullv -> true,
    )

    checkLaws("closure lattice laws")(
      Domain[CloTy, String](
        cloTys,
        cloNames,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    // ------------------------------------------------------------------------
    // regression examples that the laws alone would not pin down
    // ------------------------------------------------------------------------
    checkEqual("or")(
      (ObjectT || FunctionT) -> ObjectT,
      (ReturnT || AbruptT("break")) -> (ReturnT || BreakT),
      (FunctionT || FunctionT) -> FunctionT,
      (FunctionT || RecordT("ECMAScriptFunctionObject")) -> FunctionT,
    )

    checkEqual("and")(
      (ObjectT && FunctionT) -> FunctionT,
      (AbruptT && ReturnT) -> ReturnT,
      (FunctionT && FunctionT) -> FunctionT,
      (ConstructorT && RecordT("ECMAScriptFunctionObject")) ->
      RecordT("ECMAScriptFunctionObject", List("Construct")),
      (FunctionT && RecordT("ProxyExoticObject")) ->
      RecordT("ProxyExoticObject", List("Call")),
    )

    checkEqual("order")(
      (ReturnT <= AbruptT("break")) -> false,
    )

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
  }

  init
}
