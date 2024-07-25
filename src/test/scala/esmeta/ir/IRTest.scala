package esmeta
package ir

import esmeta.ty.*

/** test for IR */
trait IRTest extends ESMetaTest {
  def category: String = "ir"
}
object IRTest {
  // tests for IR parser
  def parseTest(program: Program): Program = {
    val newProgram = Program.from(program.toString)
    assert(program == newProgram)
    program
  }
  def parseFileTest(filename: String): Program =
    parseTest(Program.fromFile(filename))

  // functions
  lazy val mainFunc = Func(true, FuncKind.AbsOp, "f", params, ty, seq)
  lazy val func = Func(false, FuncKind.AbsOp, "f", params, ty, seq)

  // parameters
  lazy val params = List(xParam, yParam)
  lazy val xParam = Param(x, ty, false)
  lazy val yParam = Param(y, ty, true)

  // instructions
  lazy val xExprInst = IExpr(xExpr)
  lazy val let = ILet(x, empty)
  lazy val del = IDelete(field)
  lazy val pushFront = IPush(xExpr, yExpr, true)
  lazy val pushBack = IPush(xExpr, yExpr, false)
  lazy val ret = IReturn(xExpr)
  lazy val assertInst = IAssert(xExpr)
  lazy val print = IPrint(xExpr)
  lazy val assign = IAssign(field, xExpr)
  lazy val nop = INop()
  lazy val seq = ISeq(List(let, del, ret))
  lazy val emptySeq = ISeq(List())
  lazy val ifInst = IIf(
    xExpr,
    seq,
    emptySeq,
  )
  lazy val ifElseInst = IIf(
    xExpr,
    seq,
    seq,
  )
  lazy val whileInst = IWhile(
    xExpr,
    seq,
  )
  lazy val call = ICall(temp, xExpr, List(xExpr, yExpr))
  lazy val sdoCall = ISdoCall(temp, xExpr, "Evaluation", List(xExpr, yExpr))

  // expressions
  lazy val comp = EComp(normal, xExpr, empty)
  lazy val isComp = EIsCompletion(xExpr)
  lazy val riaCheck = EReturnIfAbrupt(xExpr, true)
  lazy val riaNoCheck = EReturnIfAbrupt(xExpr, false)
  lazy val popFront = EPop(xExpr, true)
  lazy val popBack = EPop(xExpr, false)
  lazy val parse = EParse(xExpr, nt)
  lazy val nt = ENt("A", List(true, false))
  lazy val yet = EYet("NOT YET")
  lazy val contains = EContains(xExpr, xExpr)
  lazy val substring = ESubstring(xExpr, xExpr, None)
  lazy val substringTo = ESubstring(xExpr, xExpr, Some(xExpr))
  lazy val trim = ETrim(ETrim(xExpr, true), false)
  lazy val trimStart = ETrim(xExpr, true)
  lazy val trimEnd = ETrim(xExpr, false)
  lazy val xExpr = ERef(x)
  lazy val yExpr = ERef(y)
  lazy val unary = EUnary(UOp.Neg, xExpr)
  lazy val binary = EBinary(BOp.Add, xExpr, xExpr)
  lazy val variadic = EVariadic(VOp.Min, List(xExpr, xExpr, xExpr))
  lazy val mathOp = EMathOp(MOp.Tan, List(xExpr))
  lazy val convert = EConvert(COp.ToBigInt, xExpr)
  lazy val typeOf = ETypeOf(xExpr)
  lazy val typeCheck = ETypeCheck(xExpr, EStr(ty.toString))
  // debugging expressions
  lazy val debug = EDebug(xExpr)
  // random number expressions
  lazy val rand = ERandom()
  // AST expressions
  lazy val ast = ESyntactic("Identifier", Nil, 1, Nil)
  lazy val astArgs = ESyntactic("Identifier", List(true, false), 1, Nil)
  lazy val astSingle =
    ESyntactic("Identifier", List(true, false), 1, List(Some(xExpr)))
  lazy val astMultiple = ESyntactic(
    "Identifier",
    List(true, false),
    1,
    List(Some(xExpr), Some(yExpr)),
  )
  lazy val astComplex = ESyntactic(
    "Identifier",
    List(true, false),
    3,
    List(None, Some(xExpr), None, Some(yExpr)),
  )
  lazy val lex = ELexical("Identifier", xExpr)
  // allocation expressions
  lazy val rec = ERecord("T", List("A" -> EBool(true), "B" -> EAbsent()))
  lazy val list = EList(List(EUndef(), ENull(), EAbsent()))
  lazy val symbol = ERecord("Symbol", List("Description" -> EUndef()))
  lazy val copy = ECopy(xExpr)
  lazy val keys = EKeys(xExpr, false)
  lazy val keysInt = EKeys(xExpr, true)
  lazy val getChildren = EGetChildren(xExpr)
  lazy val getItems = EGetItems(xExpr, xExpr)
  def assignASite(e: AllocExpr, k: Int): AllocExpr = { e.asite = k; e }
  lazy val recASite = assignASite(rec.copy(), 3)
  lazy val listASite = assignASite(list.copy(), 1)
  lazy val symbolASite = assignASite(symbol.copy(), 7)
  lazy val copyASite = assignASite(copy.copy(), 42)
  lazy val keysASite = assignASite(keys.copy(), 5)
  lazy val keysIntASite = assignASite(keysInt.copy(), 6)
  lazy val getChildrenASite = assignASite(getChildren.copy(), 9)
  lazy val getItemsASite = assignASite(getItems.copy(), 10)
  // literals
  lazy val normal = EEnum("normal")
  lazy val empty = EEnum("empty")
  lazy val clo = EClo("f", Nil)
  lazy val cloWithCaptured = EClo("f", List(x))
  lazy val cont = ECont("g")

  // references
  lazy val global = Global("GLOBAL")
  lazy val x = Name("x")
  lazy val y = Name("y")
  lazy val temp = Temp(42)
  lazy val field = Field(x, EStr("p"))
  lazy val fieldStr = Field(x, EStr("!!"))
  lazy val fieldId = Field(x, xExpr)

  // types
  lazy val ty = Type(NumberT)
}
