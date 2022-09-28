package esmeta.ty

import esmeta.cfg.*
import esmeta.util.BaseUtils.*
import esmeta.state.{Nt, Number}
import scala.collection.mutable.ListBuffer

/** stringify test */
class StringifyTinyTest extends TyTest {
  val name: String = "tyStringifyTest"

  // registration
  def init: Unit = {
    checkParseAndStringify("Ty", Ty)(
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
      NameT("Cat") -> "Cat",
      NameT("Cat", "Dog") -> "Cat | Dog",
      RecordT("A" -> Some(NumberT), "B" -> Some(BoolT)) ->
      "{ [[A]]: Number, [[B]]: Boolean }",
      RecordT(Set("Key", "Value")) ->
      "{ [[Key]], [[Value]] }",
      RecordT("Key" -> None, "Value" -> None, "Dummy" -> Some(BotT)) ->
      "{ [[Key]], [[Value]] }",
      (ObjectT || RecordT(
        "P" -> None,
        "S" -> None,
        "Q" -> Some(NumberT),
        "R" -> Some(BoolT),
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
      NumberT -> "Number",
      NumberT(Number(Double.PositiveInfinity)) -> "Number[+INF]",
      NumberT(Number(Double.NegativeInfinity)) -> "Number[-INF]",
      NumberT(Number(Double.NaN)) -> "Number[NaN]",
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
      AbsentT -> "Absent",
    )
  }

  init
}
