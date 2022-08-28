package esmeta.ty

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.BaseUtils.*
import esmeta.state.Grammar
import scala.collection.mutable.ListBuffer

/** stringify test */
class StringifyTinyTest extends TyTest {
  val name: String = "tyStringifyTest"

  // registration
  def init: Unit = {
    lazy val func = Func(0, irFunc, Some(Block(0, ListBuffer())))
    lazy val irFunc =
      IRFunc(true, IRFunc.Kind.AbsOp, "ToNumber", Nil, Type(), ISeq(Nil))
    lazy val complexRecord =
      RecordT("A", "B") |
      RecordT(Map("A" -> NumberT, "B" -> BoolT))
    lazy val grammar = GrammarT(
      Grammar("Literal", List(true)),
      Grammar("Identifier", List(false, true, false)),
    )
    checkStringify("Ty")(
      AbruptT -> "Abrupt",
      NormalT(PureValueTy(number = true)) -> "Normal[Number]",
      SubMapT(StrTopT, RecordT("Binding")) -> "SubMap[Str |-> Record[Binding]]",
      CloTopT -> "Clo",
      CloT("ToString") -> "Clo[ToString]",
      ContT(func) -> "Cont[ToNumber]",
      ESValueT -> "ESValue",
      UnknownTy() -> "unknown",
      UnknownTy(Some("T")) -> "T",
      RecordT("A") -> "Record[A]",
      RecordT("A", "B") -> "Record[A, B]",
      RecordT(Map("A" -> NumberT, "B" -> BoolT)) ->
      "Record {A -> Number, B -> Bool}",
      complexRecord -> "Record[A, B] {A -> Number, B -> Bool}",
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
      StrTopT -> "Str",
      StrT("a") -> "Str[\"a\"]",
      BoolT -> "Bool",
      UndefT -> "Undef",
      NullT -> "Null",
      AbsentT -> "Absent",
    )
  }

  init
}
