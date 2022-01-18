package esmeta.ir

import esmeta.IR_TEST_DIR
import esmeta.ir._
import esmeta.util.SystemUtils._
import scala.collection.mutable.{Map => MMap}

class ParseAndStringifyTinyTest extends IRTest {
  val name: String = "irParseAndStringifyTest"

  // check parser from files
  def checkParseFromFiles: Unit = {
    for (file <- walkTree(IR_TEST_DIR)) {
      val filename = file.getName
      if (irFilter(filename)) check(filename) {
        val name = file.toString
        irParseTestFile(name)
      }
    }
  }

  // registration
  def init: Unit = {

    // check parser can parse *.ir files
    checkParseFromFiles

    val irMapElems = List(
      EBool(true) -> EStr("true"),
      ENull -> EStr("null"),
    )
    val sMapElems = "(true -> \"true\", null -> \"null\")"
    val irList = List(ENull, EAbsent)
    val sList = "(new [null, absent])"
    val irReturn = IReturn(EINum(4))
    val sReturn = "return 4i"
    val idList = List(Id("x"), Id("y"))
    val sIdList = "(x, y)"

    // -----------------------------------------------------------------------------
    // Instruction
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Inst", Inst)(
      IExpr(EINum(4)) -> "4i",
      ILet(Id("x"), EINum(4)) -> "let x = 4i",
      IAssign(RefId(Id("x")), ENum(3.0)) -> "x = 3.0",
      IDelete(RefId(Id("ref"))) -> "delete ref",
      IAppend(EUndef, EList(irList)) -> s"append undefined -> $sList",
      IPrepend(EUndef, EList(irList)) -> s"prepend undefined -> $sList",
      irReturn -> sReturn,
      IThrow("SyntaxError") -> "throw SyntaxError",
      IIf(EBool(true), irReturn, IExpr(ENum(3.0))) ->
        s"if true $sReturn else 3.0",
      IWhile(EBool(false), irReturn) -> s"while false $sReturn",
      ISeq(List()) -> "{}",
      ISeq(List(irReturn, IExpr(ENull))) -> s"{\n  $sReturn\n  null\n}",
      IAssert(EBool(false)) -> "assert false",
      IPrint(EBool(false)) -> "print false",
      IApp(Id("x"), EStr("f"), irList) ->
        "app x = (\"f\" null absent)",
      IWithCont(Id("x"), idList, irReturn) ->
        s"withcont x $sIdList = $sReturn",
    )

    // -----------------------------------------------------------------------------
    // Expression
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Expr", Expr)(
      ENum(3.0) -> "3.0",
      ENum(Double.PositiveInfinity) -> "Infinity",
      ENum(Double.NegativeInfinity) -> "-Infinity",
      ENum(Double.NaN) -> "NaN",
      EINum(4) -> "4i",
      EBigINum(1024) -> "1024n",
      EStr("hi") -> "\"hi\"",
      EBool(true) -> "true",
      EUndef -> "undefined",
      ENull -> "null",
      EAbsent -> "absent",
      EClo(idList, idList, IExpr(EINum(4))) ->
        s"(clo $sIdList[x, y] => 4i)",
      ECont(idList, IExpr(EINum(4))) ->
        s"(cont $sIdList [=>] 4i)",
      EMap(Ty("T"), irMapElems) -> s"(new T$sMapElems)",
      EList(irList) -> sList,
      EPop(EList(irList), EINum(0)) -> s"(pop $sList 0i)",
      ERef(RefId(Id("x"))) -> "x",
      EUOp(UOp.Neg, EINum(4)) -> "(- 4i)",
      EBOp(BOp.Div, ENum(3.0), ENum(7.0)) -> "(/ 3.0 7.0)",
      ETypeOf(EBool(false)) -> "(typeof false)",
      EIsCompletion(EINum(5)) -> "(is-completion 5i)",
      EIsInstanceOf(EBool(false), "instanceof") ->
        "(is-instance-of false instanceof)",
      EGetElems(EBool(false), "getelems") ->
        "(get-elems false getelems)",
      EGetSyntax(EAbsent) -> "(get-syntax absent)",
      EParseSyntax(EStr("code"), EStr("rule"), Nil)
        -> "(parse-syntax \"code\" \"rule\")",
      EParseSyntax(EStr("code"), EStr("rule"), List(true, false))
        -> "(parse-syntax \"code\" \"rule\" true false)",
      EConvert(ENull, COp.NumToBigInt, None) ->
        "(convert null num2bigint)",
      EConvert(EStr("4"), COp.StrToNum, Some(EAbsent)) ->
        "(convert \"4\" str2num absent)",
      EContains(EList(irList), ENull) -> s"(contains $sList null)",
      EReturnIfAbrupt(ENum(3.0), true) -> "[? 3.0]",
      EReturnIfAbrupt(ENum(3.0), false) -> "[! 3.0]",
      ECopy(EStr("obj")) -> "(copy-obj \"obj\")",
      EKeys(EStr("obj"), false) -> "(map-keys \"obj\")",
      EKeys(EStr("obj"), true) -> "(map-keys \"obj\" [int-sorted])",
      ENotSupported("hi") -> "??? \"hi\"",
    )

    // -----------------------------------------------------------------------------
    // Reference, Types, Operators
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Ref", Ref)(
      RefId(Id("y")) -> "y",
      RefProp(RefId(Id("z")), EStr("w")) -> "z.w",
      RefProp(RefId(Id("x")), ENum(3.0)) -> "x[3.0]",
    )
    checkParseAndStringify("Ty", Ty)(Ty("T") -> "T")
    checkParseAndStringify("Id", Id)(Id("x") -> "x")
    checkParseAndStringify("UOp", UOp)(
      UOp.Neg -> "-",
      UOp.Not -> "!",
      UOp.BNot -> "~",
    )
    checkParseAndStringify("BOp", BOp)(
      BOp.Plus -> "+",
      BOp.Sub -> "-",
      BOp.Mul -> "*",
      BOp.Pow -> "**",
      BOp.Div -> "/",
      BOp.UMod -> "%%",
      BOp.Mod -> "%",
      BOp.Eq -> "=",
      BOp.Equal -> "==",
      BOp.And -> "&&",
      BOp.Or -> "||",
      BOp.Xor -> "^^",
      BOp.BAnd -> "&",
      BOp.BOr -> "|",
      BOp.BXOr -> "^",
      BOp.LShift -> "<<",
      BOp.Lt -> "<",
      BOp.URShift -> ">>>",
      BOp.SRShift -> ">>",
    )
    checkParseAndStringify("COp", COp)(
      COp.StrToNum -> "str2num",
      COp.StrToBigInt -> "str2bigint",
      COp.NumToStr -> "num2str",
      COp.NumToInt -> "num2int",
      COp.NumToBigInt -> "num2bigint",
      COp.BigIntToNum -> "bigint2num",
    )

    // -----------------------------------------------------------------------------
    // State
    // -----------------------------------------------------------------------------
    checkStringify("State")(
      State() -> """{
      |  context: {
      |    name: TOP_LEVEL
      |    return: RETURN
      |    cursor: [EMPTY]
      |    local-vars: {}
      |  }
      |  context-stack: []
      |  globals: {}
      |  heap: (SIZE = 0): {}
      |}""".stripMargin,
    )
    checkStringify("Context")(
      Context() -> """{
      |  name: TOP_LEVEL
      |  return: RETURN
      |  cursor: [EMPTY]
      |  local-vars: {}
      |}""".stripMargin,
    )
    checkStringify("Heap")(
      Heap(
        MMap(NamedAddr("namedaddr") -> IRSymbol(Num(3.0))),
        1,
      ) -> """(SIZE = 1): {
      |  #namedaddr -> (Symbol 3.0)
      |}""".stripMargin,
    )
    checkStringify("Obj")(
      IRSymbol(Str("const")) -> """(Symbol "const")""",
      IRMap(
        Ty("T"),
        MMap(Num(3.0) -> IRMapValue(Num(2.0), 4)),
        1,
      ) -> """(TYPE = T) {
      |  3.0 -> 2.0
      |}""".stripMargin,
      IRList(Vector(Num(3.0))) -> "[3.0]",
      IRList(Vector(Num(3.0), INum(42))) -> "[3.0, 42i]",
      IRNotSupported("tyname", "desc") -> """(NotSupported "tyname" "desc")""",
    )
    checkStringify("Value")(
      Num(3.0) -> "3.0",
      Num(Double.NaN) -> "NaN",
      Num(Double.PositiveInfinity) -> "Infinity",
      Num(Double.NegativeInfinity) -> "-Infinity",
      INum(2) -> "2i",
      BigINum(BigInt("1920380182930189023")) -> "1920380182930189023n",
      Str("hello") -> """"hello"""",
      Bool(true) -> "true",
      Bool(false) -> "false",
      Undef -> "undefined",
      Null -> "null",
      Absent -> "absent",
    )
    checkStringify("Addr")(
      NamedAddr("GLOBAL") -> "#GLOBAL",
      DynamicAddr(3) -> "#3",
    )
    checkStringify("Clo")(
      Clo("clo", idList, MMap[Id, Value](Id("z") -> Num(3.0)), None) ->
        "clo:closure(x, y)[z -> 3.0] => ...",
    )
    checkStringify("Cont")(
      Cont(idList, Context(), List()) -> "TOP_LEVEL(x, y) [=>] ...",
    )
    checkStringify("RefValue")(
      RefValueId(Id("x")) -> "x",
      RefValueProp(NamedAddr("namedaddr"), Num(3.0)) -> "#namedaddr[3.0]",
      RefValueProp(
        NamedAddr("namedaddr"),
        Str("abc"),
      ) -> """#namedaddr["abc"]""",
      RefValueProp(Str("hello"), INum(3)) -> """"hello"[3i]""",
    )
  }

  init
}
