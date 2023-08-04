package esmeta.ty

import esmeta.ty.util.JsonProtocol.given
import esmeta.state.{Nt, Number, ExtMath, Math, MathInf}
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*

/** JSON test */
class JsonTinyTest extends TyTest {
  val name: String = "tyJsonTest"

  // registration
  def init: Unit = {
    checkJsonWithString("Ty")(
      AnyT -> "Any",
      PureValueT -> "PureValue",
      AbruptT -> "Abrupt",
      NormalT(NumberT) -> "Normal[Number]",
      SubMapT(
        StrT,
        NameT("Binding"),
      ) -> "SubMap[String |-> Binding]",
      CloT -> "Clo",
      CloT("ToString:clo0") -> "Clo[\"ToString:clo0\"]",
      ContT -> "Cont",
      ContT(42, 3) -> "Cont[3, 42]",
      ESValueT -> "ESValue",
      UnknownTy() -> "Unknown",
      UnknownTy(Some("T")) -> "Unknown[\"T\"]",
      NameT -> "AnyName",
      NameT("Cat") -> "Cat",
      NameT("Cat", "Dog") -> "Cat | Dog",
      RecordT -> "AnyRecord",
      RecordT("A" -> NumberT, "B" -> BoolT) ->
      "{ [[A]]: Number, [[B]]: Boolean }",
      RecordT(Set("Key", "Value")) ->
      "{ [[Key]], [[Value]] }",
      RecordT("Key" -> ValueTy.Top, "Value" -> ValueTy.Top, "Dummy" -> BotT) ->
      "{ [[Key]], [[Value]] }",
      (ObjectT || RecordT(
        "P" -> ValueTy.Top,
        "S" -> ValueTy.Top,
        "Q" -> NumberT,
        "R" -> BoolT,
      )) -> "Object | { [[P]], [[Q]]: Number, [[R]]: Boolean, [[S]] }",
      NilT -> "Nil",
      ListT(NumberT) -> "List[Number]",
      SymbolT -> "Symbol",
      AstT -> "Ast",
      AstT("Literal") -> "Ast[Literal]",
      AstSingleT("Member", 1, 3) -> "Ast:Member[1,3]",
      NtT(
        Nt("Literal", List(true)),
        Nt("Identifier", List(false, true, false)),
      ) -> "Nt[|Identifier|[FTF], |Literal|[T]]",
      CodeUnitT -> "CodeUnit",
      ConstT("key") -> "Const[~key~]",
      ConstT("key", "value") -> "Const[~key~, ~value~]",
      MathT -> "Math",
      MathT(0, 1) -> "Math[0, 1]",
      MathInfT(true) -> "Math[+INF]",
      MathInfT(false) -> "Math[-INF]",
      ExtMathT -> "ExtMath",
      NumberT -> "Number",
      NumberT(Number(Double.PositiveInfinity)) -> "Number[+INF_F]",
      NumberT(Number(Double.NegativeInfinity)) -> "Number[-INF_F]",
      NumberT(Number(Double.NaN)) -> "Number[NaN]",
      NumberT(
        Number(Double.PositiveInfinity),
        Number(Double.NegativeInfinity),
        Number(Double.NaN),
        Number(-0.0),
        Number(0.0),
      ) -> "Number[-INF_F, -0.0, 0.0, +INF_F, NaN]",
      BigIntT -> "BigInt",
      StrT -> "String",
      StrT("a") -> "String[\"a\"]",
      BoolT -> "Boolean",
      UndefT -> "Undefined",
      NullT -> "Null",
      AbsentT -> "Absent",
    )
  }

  init
}
