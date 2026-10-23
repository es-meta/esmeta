package esmeta.ty

import esmeta.cfg.*
import esmeta.util.BaseUtils.*
import esmeta.state.{GrammarSymbol, Math, Number}
import scala.collection.mutable.ListBuffer

/** stringify test */
class StringifyTinyTest extends TyTest {
  val name: String = "tyStringifyTest"

  // registration
  def init: Unit = {

    checkParseAndStringify("TyModel", TyModel)(
      tyModel0 -> "",
      tyModel1 -> """type A""",
      tyModel2 -> """type A extends B
      |
      |type A {
      |  abstract def a;
      |}""".stripMargin,
      tyModel3 -> """type A
      |
      |type A = B {
      |  abstract def a;
      |}
      |
      |type A {
      |  abstract def a;
      |  def c?;
      |}""".stripMargin,
    )

    checkParseAndStringify("TyDecl", TyDecl)(
      decl0 -> """type A""",
      decl1 -> """type A {
      |  abstract def a;
      |}""".stripMargin,
      decl2 -> """type A {
      |  abstract def a;
      |  def c?;
      |}""".stripMargin,
      declParent0 -> """type A extends B""",
      declParent1 -> """type A = B {
      |  abstract def a;
      |}""".stripMargin,
      declParent2 -> """type A extends B {
      |  abstract def a;
      |  def c?;
      |}""".stripMargin,
    )

    checkParseAndStringify("TyDecl.Elem", TyDecl.Elem)(
      absMethod -> "abstract def a",
      conMethod -> "def b",
      conMethodOpt -> "def c?",
      conMethodTarget -> "def d = foo",
      conMethodOptTarget -> "def e? = bar",
    )

    checkParseAndStringify("FieldMap", FieldMap)(
      fieldMap0 -> """{}""",
      fieldMap1 -> """{
      |  p
      |}""".stripMargin,
      fieldMap2 -> """{
      |  p
      |  q: Boolean
      |  r?: Undefined
      |}""".stripMargin,
    )

    checkParseAndStringify("Ty", Ty)(
      AnyT -> "Any",
      CompT -> "Completion",
      AbruptT -> "Abrupt",
      AbruptT("return", "throw") -> "Abrupt[return, throw]",
      NormalT(NumberT) -> "Normal[Number]",
      MapT -> "Map",
      MapT(StrT, RecordT("Binding")) -> "Map[String -> Record[Binding]]",
      CloT -> "Clo",
      CloT(List(NumberT, BoolT), StrT) -> "Clo[(Number, Boolean) => String]",
      CloT("ToString:clo0") -> "Clo[\"ToString:clo0\"]",
      ContT -> "Cont",
      ContT(42, 3) -> "Cont[3, 42]",
      ESValueT -> "ESValue",
      UnknownTy() -> "Unknown",
      UnknownTy(Some("T")) -> "Unknown[\"T\"]",
      RecordT -> "Record",
      RecordT("Cat") -> "Record[Cat]",
      RecordT("Cat", "Dog") -> "Record[Cat | Dog]",
      RecordT("Object", Map("PrivateElements" -> NilT)) ->
      "Record[Object { PrivateElements: Nil }]",
      RecordT(
        "",
        Map(
          "P" -> AnyT,
          "S" -> AnyT,
          "Q" -> NumberT,
          "R" -> BoolT,
        ),
      ) -> "Record[{ P, Q: Number, R: Boolean, S }]",
      FunctionT -> "Record[FunctionObject]",
      ConstructorT -> "Record[Constructor]",
      NilT -> "Nil",
      ListT(NumberT) -> "List[Number]",
      SymbolT -> "Record[Symbol]",
      AstT -> "Ast",
      AstT("Literal") -> "Ast[Literal]",
      AstT("Member", 1) -> "Ast[Member[1]]",
      GrammarSymbolT(
        GrammarSymbol("Literal", List(true)),
        GrammarSymbol("Identifier", List(false, true, false)),
      ) -> "GrammarSymbol[|Identifier|[FTF], |Literal|[T]]",
      CodeUnitT -> "CodeUnit",
      EnumT("key") -> "Enum[~key~]",
      EnumT("key", "value") -> "Enum[~key~, ~value~]",
      MathT -> "Math",
      IntT -> "Int",
      NonPosIntT -> "Int[NonPos]",
      NonNegIntT -> "Int[NonNeg]",
      NegIntT -> "Int[Neg]",
      PosIntT -> "Int[Pos]",
      IntT(0, 1) -> "Int[0, 1]",
      ValueTy(math = MathIntTy(IntSignTy(Sign.NonZero))) -> "Int[NonZero]",
      ValueTy(math = MathTy.Zero) -> "Int[0]",
      InfinityT -> "INF",
      NegInfinityT -> "-INF",
      PosInfinityT -> "+INF",
      ValueTy(math = MathTy.NonNeg) -> "Math[NonNeg]",
      ValueTy(math = MathTy.NonPos) -> "Math[NonPos]",
      ValueTy(math = MathTy.Neg) -> "Math[Neg]",
      ValueTy(math = MathTy.Pos) -> "Math[Pos]",
      ValueTy(math = MathSignTy(Sign.NonZero)) -> "Math[NonZero]",
      ValueTy(math =
        MathSetTy(Set(Math(-1.5), Math(0.5))),
      ) -> "Math[-1.5, 0.5]",
      NumberT -> "Number",
      ValueTy(number = NumberTy.Finite) -> "Number[Finite]",
      ValueTy(number =
        NumberTy.Finite || NumberTy.NaN,
      ) -> "Number[Finite, NaN]",
      ValueTy(number = NumberTy.finite(FinNumberSignTy(Sign.Pos))) ->
      "Number[Pos]",
      ValueTy(number = NumberTy.finite(FinNumberSignTy(Sign.NonZero))) ->
      "Number[NonZero]",
      ValueTy(number = NumberTy.int(IntSignTy(Sign.NonZero))) ->
      "Number[Int[NonZero]]",
      ValueTy(number = NumberTy.Int || NumberTy.PosInf) -> "Number[Int, +INF]",
      ValueTy(number = NumberTy.NegInf || NumberTy.NaN) -> "Number[-INF, NaN]",
      NumberIntT -> "Number[Int]",
      NumberNonPosIntT -> "Number[Int[NonPos]]",
      NumberNonNegIntT -> "Number[Int[NonNeg]]",
      NumberNegIntT -> "Number[Int[Neg]]",
      NumberPosIntT -> "Number[Int[Pos]]",
      (NumberNonNegIntT || NaNT) -> "Number[Int[NonNeg], NaN]",
      PosNumberT -> "Number[Pos, +INF]",
      NegNumberT -> "Number[Neg, -INF]",
      NonNegNumberT -> "Number[NonNeg, +INF]",
      NonPosNumberT -> "Number[NonPos, -INF]",
      NonZeroNumberT -> "Number[NonZero, -INF, +INF, NaN]",
      InfiniteNumberT -> "Number[-INF, +INF]",
      NumberT(Number(Double.PositiveInfinity)) -> "Number[+INF]",
      NumberT(Number(Double.NegativeInfinity)) -> "Number[-INF]",
      NumberT(Number(Double.NaN)) -> "Number[NaN]",
      NumberT(Number(Double.PositiveInfinity), Number(Double.NaN)) ->
      "Number[+INF, NaN]",
      ValueTy(number = NumberTy.Int || NumberTy.Infinite) ->
      "Number[Int, -INF, +INF]",
      ValueTy(number = NumberTy.Int || NumberTy.PosInf || NumberTy.NaN) ->
      "Number[Int, +INF, NaN]",
      NumberT(Number(0.0)) -> "Number[0.0]",
      NumberT(Number(-0.0)) -> "Number[-0.0]",
      NumberT(
        Number(Double.PositiveInfinity),
        Number(Double.NegativeInfinity),
        Number(Double.NaN),
        Number(-0.0),
        Number(0.0),
      ) -> "Number[-0.0, 0.0, -INF, +INF, NaN]",
      BigIntT -> "BigInt",
      StrT -> "String",
      StrT("a") -> "String[\"a\"]",
      BoolT -> "Boolean",
      UndefT -> "Undefined",
      NullT -> "Null",
    )
  }

  init
}
