package esmeta.ty

import esmeta.cfg.*
import esmeta.util.BaseUtils.*
import esmeta.state.Grammar
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
        StrTopT,
        NameT("Binding"),
      ) -> "SubMap[String |-> Binding]",
      CloTopT -> "Clo",
      CloT("ToString:clo0") -> "Clo[\"ToString:clo0\"]",
      ContTopT -> "Cont",
      ContT("ToNumber:cont0") -> "Cont[\"ToNumber:cont0\"]",
      ESValueT -> "ESValue",
      UnknownTy() -> "Unknown",
      UnknownTy(Some("T")) -> "Unknown[\"T\"]",
      NameT("Cat") -> "Cat",
      NameT("Cat", "Dog") -> "Cat | Dog",
      RecordT(map = Map("A" -> NumberT, "B" -> BoolT)) ->
      "{ [[A]]: Number, [[B]]: Boolean }",
      RecordT(fields = Set("Key", "Value"), Map()) ->
      "{ [[Key]], [[Value]] }",
      RecordT(Set("Key", "Value"), Map("Dummy" -> BotT)) ->
      "{ [[Key]], [[Value]] }",
      (ObjectT | RecordT(
        fields = Set("P", "S"),
        map = Map("Q" -> NumberT, "R" -> BoolT),
      )) -> "Object | { [[P]], [[Q]]: Number, [[R]]: Boolean, [[S]] }",
      NilT -> "Nil",
      ListT(NumberT) -> "List[Number]",
      SymbolT -> "Symbol",
      AstTopT -> "Ast",
      AstT("Literal") -> "Ast[Literal]",
      GrammarT(
        Grammar("Literal", List(true)),
        Grammar("Identifier", List(false, true, false)),
      ) -> "Grammar[|Identifier|[FTF], |Literal|[T]]",
      CodeUnitT -> "CodeUnit",
      ConstT("key") -> "Const[~key~]",
      ConstT("key", "value") -> "Const[~key~, ~value~]",
      MathT -> "Math",
      NumberT -> "Number",
      BigIntT -> "BigInt",
      StrTopT -> "String",
      StrT("a") -> "String[\"a\"]",
      BoolT -> "Boolean",
      UndefT -> "Undefined",
      NullT -> "Null",
      AbsentT -> "Absent",
    )
  }

  init
}
