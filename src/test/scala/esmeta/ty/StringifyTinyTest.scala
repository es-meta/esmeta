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
    lazy val complexRecord =
      RecordT("A", "B") |
      RecordT(Map("A" -> NumberT, "B" -> BoolT))
    lazy val grammar = GrammarT(
      Grammar("Literal", List(true)),
      Grammar("Identifier", List(false, true, false)),
    )
    checkParseAndStringify("Ty", Ty)(
      AbruptT -> "Abrupt",
      NormalT(PureValueTy(number = true)) -> "Normal[Number]",
      SubMapT(
        StrTopT,
        RecordT("Binding"),
      ) -> "SubMap[String |-> Record[Binding]]",
      CloTopT -> "Clo",
      CloT("ToString:clo0") -> "Clo[\"ToString:clo0\"]",
      ContTopT -> "Cont",
      ContT("ToNumber:cont0") -> "Cont[\"ToNumber:cont0\"]",
      ESValueT -> "ESValue",
      UnknownTy() -> "Unknown",
      UnknownTy(Some("T")) -> "Unknown[\"T\"]",
      RecordT("A") -> "Record[A]",
      RecordT("A", "B") -> "Record[A, B]",
      RecordT(Map("A" -> NumberT, "B" -> BoolT)) ->
      "Record {\"A\" -> Number, \"B\" -> Boolean}",
      complexRecord -> "Record[A, B] {\"A\" -> Number, \"B\" -> Boolean}",
      NilT -> "Nil",
      ListT(NumberT) -> "List[Number]",
      SymbolT -> "Symbol",
      AstTopT -> "Ast",
      AstT("Literal") -> "Ast[Literal]",
      grammar -> "Grammar[|Identifier|[FTF], |Literal|[T]]",
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
