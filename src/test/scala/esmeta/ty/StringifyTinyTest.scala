package esmeta.ty

import esmeta.cfg.*
import esmeta.util.BaseUtils.*
import esmeta.state.{GrammarSymbol, Number}
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
      |  q : [U] Boolean
      |}""".stripMargin,
      fieldMap3 -> """{
      |  p
      |  q : [A] Boolean
      |  r : [UA] Null
      |}""".stripMargin,
    )

    checkParseAndStringify("Ty", Ty)(
      AnyT -> "Any",
      CompT -> "Completion",
      AbruptT -> "Abrupt",
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
      "Record[Object { PrivateElements : Nil }]",
      RecordT(
        "",
        Map(
          "P" -> AnyT,
          "S" -> AnyT,
          "Q" -> NumberT,
          "R" -> BoolT,
        ),
      ) -> "Record[{ P, Q : Number, R : Boolean, S }]",
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
      NonPosIntT -> "Int[-0]",
      NonNegIntT -> "Int[0+]",
      NegIntT -> "Int[-]",
      PosIntT -> "Int[+]",
      IntT(0, 1) -> "Int[0, 1]",
      InfinityT -> "INF",
      NegInfinityT -> "-INF",
      PosInfinityT -> "+INF",
      NumberT -> "Number",
      NumberIntT -> "NumberInt",
      NumberNonPosIntT -> "NumberInt[-0]",
      NumberNonNegIntT -> "NumberInt[0+]",
      NumberNegIntT -> "NumberInt[-]",
      NumberPosIntT -> "NumberInt[+]",
      (NumberNonNegIntT || NaNT) -> "NumberInt[0+] | NaN",
      NumberT(Number(Double.PositiveInfinity)) -> "Number[+INF]",
      NumberT(Number(Double.NegativeInfinity)) -> "Number[-INF]",
      NumberT(Number(Double.NaN)) -> "NaN",
      NumberT(
        Number(Double.PositiveInfinity),
        Number(Double.NegativeInfinity),
        Number(Double.NaN),
        Number(-0.0),
        Number(0.0),
      ) -> "Number[-INF, -0.0, 0.0, +INF, NaN]",
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
