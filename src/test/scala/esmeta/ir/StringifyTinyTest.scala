package esmeta.ir

import esmeta.IR_TEST_DIR
import esmeta.ir.*
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.{Loc, Pos}
import esmeta.util.SystemUtils._
import scala.collection.mutable.ListBuffer

/** stringify test */
class StringifyTinyTest extends IRTest {
  val name: String = "irStringifyTest"

  // registration
  def init: Unit = {
    // check parser and stringifier from files
    for (file <- walkTree(IR_TEST_DIR)) {
      val filename = file.getName
      if (irFilter(filename)) check(filename) {
        val name = file.toString
        IRTest.parseFileTest(name)
      }
    }

    // -------------------------------------------------------------------------
    // functions
    // -------------------------------------------------------------------------
    lazy val mainFunc = Func(true, FuncKind.AbsOp, "f", params, ty, seq)
    lazy val func = Func(false, FuncKind.AbsOp, "f", params, ty, seq)

    // tests
    checkParseAndStringify("Func", Func)(
      mainFunc -> """@main def f(
      |  x: Number,
      |  y?: Number,
      |): Number {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
      func -> """def f(
      |  x: Number,
      |  y?: Number,
      |): Number {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // function kinds
    // -------------------------------------------------------------------------
    checkParseAndStringify("FuncKind", FuncKind)(
      FuncKind.AbsOp -> "",
      FuncKind.NumMeth -> "<NUM>:",
      FuncKind.SynDirOp -> "<SYNTAX>:",
      FuncKind.ConcMeth -> "<CONC>:",
      FuncKind.InternalMeth -> "<INTERNAL>:",
      FuncKind.Builtin -> "<BUILTIN>:",
      FuncKind.Clo -> "<CLO>:",
      FuncKind.Cont -> "<CONT>:",
    )

    // -------------------------------------------------------------------------
    // parameters
    // -------------------------------------------------------------------------
    lazy val params = List(xParam, yParam)
    lazy val xParam = Param(x, ty, false)
    lazy val yParam = Param(y, ty, true)

    // tests
    checkParseAndStringify("Param", Param)(
      xParam -> "x: Number",
      yParam -> "y?: Number",
    )

    // -------------------------------------------------------------------------
    // instructions
    // -------------------------------------------------------------------------
    lazy val xExprInst = IExpr(xExpr)
    lazy val let = ILet(x, empty)
    lazy val del = IDelete(prop)
    lazy val pushFront = IPush(xExpr, yExpr, true)
    lazy val pushBack = IPush(xExpr, yExpr, false)
    lazy val ret = IReturn(xExpr)
    lazy val assert = IAssert(xExpr)
    lazy val print = IPrint(xExpr)
    lazy val assign = IAssign(prop, xExpr)
    lazy val nop = INop()
    lazy val seq = ISeq(List(let, del, ret))
    lazy val emptySeq = ISeq(List())
    lazy val ifInst = IIf(
      xExpr,
      seq,
      emptySeq,
    )
    lazy val loopInst = ILoop(
      "repeat",
      xExpr,
      seq,
    )
    lazy val call = ICall(temp, xExpr, List(xExpr, yExpr))
    lazy val methodCall = IMethodCall(temp, x, "Get", List(xExpr, yExpr))
    lazy val sdoCall = ISdoCall(temp, xExpr, "Evaluation", List(xExpr, yExpr))

    // tests
    checkParseAndStringify("Inst", Inst)(
      xExprInst -> "x",
      let -> "let x = ~empty~",
      del -> "delete x.p",
      pushFront -> "push x > y",
      pushBack -> "push y < x",
      ret -> "return x",
      assert -> "assert x",
      print -> "print x",
      assign -> "x.p = x",
      nop -> "nop",
      seq -> """{
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
      emptySeq -> "{}",
      ifInst -> """if x {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |} else {}""".stripMargin,
      loopInst -> """loop[repeat] x {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
      call -> "call %42 = x(x, y)",
      methodCall -> "method-call %42 = x->Get(x, y)",
      sdoCall -> "sdo-call %42 = x->Evaluation(x, y)",
    )

    // -------------------------------------------------------------------------
    // expressions
    // -------------------------------------------------------------------------
    lazy val comp = EComp(normal, xExpr, empty)
    lazy val isComp = EIsCompletion(xExpr)
    lazy val riaCheck = EReturnIfAbrupt(xExpr, true)
    lazy val riaNoCheck = EReturnIfAbrupt(xExpr, false)
    lazy val popFront = EPop(xExpr, true)
    lazy val popBack = EPop(xExpr, false)
    lazy val parse = EParse(xExpr, nt)
    lazy val nt = ENt("A", List(true, false))
    lazy val yet = EYet("NOT YET")
    lazy val contains = EContains(xExpr, xExpr, None)
    lazy val containsField = EContains(xExpr, xExpr, Some(ty, "Value"))
    lazy val substring = ESubstring(xExpr, xExpr, None)
    lazy val substringTo = ESubstring(xExpr, xExpr, Some(xExpr))
    lazy val xExpr = ERef(x)
    lazy val yExpr = ERef(y)
    lazy val unary = EUnary(UOp.Neg, xExpr)
    lazy val binary = EBinary(BOp.Add, xExpr, xExpr)
    lazy val variadic = EVariadic(VOp.Min, List(xExpr, xExpr, xExpr))
    lazy val mathOp = EMathOp(MOp.Tan, List(xExpr))
    lazy val clamp = EClamp(xExpr, xExpr, xExpr)
    lazy val convert = EConvert(COp.ToBigInt, xExpr)
    lazy val typeOf = ETypeOf(xExpr)
    lazy val typeCheck = ETypeCheck(xExpr, EStr(ty.toString))
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
    lazy val rec =
      EMap("T", List(EUndef -> EBool(true), ENull -> EAbsent))
    lazy val list = EList(List(EUndef, ENull, EAbsent))
    lazy val symbol = ESymbol(ENull)
    lazy val copy = ECopy(xExpr)
    lazy val keys = EKeys(xExpr, false)
    lazy val keysInt = EKeys(xExpr, true)
    def assignASite(e: AllocExpr, k: Int): AllocExpr = { e.asite = k; e }
    lazy val recASite = assignASite(rec.copy(), 3)
    lazy val listASite = assignASite(list.copy(), 1)
    lazy val symbolASite = assignASite(symbol.copy(), 7)
    lazy val copyASite = assignASite(copy.copy(), 42)
    lazy val keysASite = assignASite(keys.copy(), 5)
    lazy val keysIntASite = assignASite(keysInt.copy(), 6)
    // literals
    lazy val normal = EConst("normal")
    lazy val empty = EConst("empty")
    lazy val clo = EClo("f", Nil)
    lazy val cloWithCaptured = EClo("f", List(x))
    lazy val cont = ECont("g")

    // tests
    checkParseAndStringify("Expr", Expr)(
      comp -> "comp[~normal~/~empty~](x)",
      isComp -> "(comp? x)",
      riaCheck -> "[? x]",
      riaNoCheck -> "[! x]",
      popFront -> "(pop < x)",
      popBack -> "(pop > x)",
      parse -> "(parse x (nt |A|[TF]))",
      nt -> "(nt |A|[TF])",
      yet -> "(yet \"NOT YET\")",
      contains -> "(contains x x)",
      containsField -> "(contains x x: Number Value)",
      substring -> "(substring x x)",
      substringTo -> "(substring x x x)",
      xExpr -> "x",
      unary -> "(- x)",
      binary -> "(+ x x)",
      variadic -> "(min x x x)",
      mathOp -> "([math:tan] x)",
      clamp -> "(clamp x x x)",
      convert -> "([bigInt] x)",
      typeOf -> "(typeof x)",
      typeCheck -> "(? x: \"Number\")",
      // AST expressions
      ast -> "|Identifier|<1>",
      astArgs -> "|Identifier|[TF]<1>",
      astSingle -> "|Identifier|[TF]<1>(x)",
      astMultiple -> "|Identifier|[TF]<1>(x, y)",
      astComplex -> "|Identifier|[TF]<3>(, x, , y)",
      lex -> "|Identifier|(x)",
      // allocation expressions
      rec -> "(new T(undefined -> true, null -> absent))",
      list -> "(new [undefined, null, absent])",
      symbol -> "(new 'null)",
      copy -> "(copy x)",
      keys -> "(keys x)",
      keysInt -> "(keys-int x)",
      // allocation expressions with allocation sites
      recASite -> "(new T(undefined -> true, null -> absent))[#3]",
      listASite -> "(new [undefined, null, absent])[#1]",
      symbolASite -> "(new 'null)[#7]",
      copyASite -> "(copy x)[#42]",
      keysASite -> "(keys x)[#5]",
      keysIntASite -> "(keys-int x)[#6]",
      // literals
      EMathVal(4) -> "4",
      ENumber(3.0) -> "3.0f",
      ENumber(Double.PositiveInfinity) -> "+INF",
      ENumber(Double.NegativeInfinity) -> "-INF",
      ENumber(Double.NaN) -> "NaN",
      EBigInt(1024) -> "1024n",
      EStr("hi") -> "\"hi\"",
      EBool(true) -> "true",
      EBool(false) -> "false",
      EUndef -> "undefined",
      ENull -> "null",
      EAbsent -> "absent",
      normal -> "~normal~",
      empty -> "~empty~",
      clo -> "clo<f>",
      cloWithCaptured -> "clo<f, [x]>",
      cont -> "cont<g>",
    )

    // -------------------------------------------------------------------------
    // operators
    // -------------------------------------------------------------------------
    checkParseAndStringify("UOp", UOp)(
      UOp.Neg -> "-",
      UOp.Not -> "!",
      UOp.BNot -> "~",
    )
    checkParseAndStringify("BOp", BOp)(
      BOp.Add -> "+",
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
    checkParseAndStringify("VOp", VOp)(
      VOp.Min -> "min",
      VOp.Max -> "max",
      VOp.Concat -> "concat",
    )
    checkParseAndStringify("MOp", MOp)(
      MOp.Expm1 -> "[math:expm1]",
      MOp.Log10 -> "[math:log10]",
      MOp.Log2 -> "[math:log2]",
      MOp.Cos -> "[math:cos]",
      MOp.Cbrt -> "[math:cbrt]",
      MOp.Exp -> "[math:exp]",
      MOp.Cosh -> "[math:cosh]",
      MOp.Sinh -> "[math:sinh]",
      MOp.Tanh -> "[math:tanh]",
      MOp.Acos -> "[math:acos]",
      MOp.Acosh -> "[math:acosh]",
      MOp.Asinh -> "[math:asinh]",
      MOp.Atanh -> "[math:atanh]",
      MOp.Asin -> "[math:asin]",
      MOp.Atan2 -> "[math:atan2]",
      MOp.Atan -> "[math:atan]",
      MOp.Log1p -> "[math:log1p]",
      MOp.Log -> "[math:log]",
      MOp.Sin -> "[math:sin]",
      MOp.Sqrt -> "[math:sqrt]",
      MOp.Tan -> "[math:tan]",
      MOp.Hypot -> "[math:hypot]",
    )
    checkParseAndStringify("COp", COp)(
      COp.ToApproxNumber -> "[approx-number]",
      COp.ToNumber -> "[number]",
      COp.ToBigInt -> "[bigInt]",
      COp.ToStr(None) -> "[str]",
      COp.ToStr(Some(xExpr)) -> "[str x]",
    )

    // -------------------------------------------------------------------------
    // references
    // -------------------------------------------------------------------------
    lazy val global = Global("GLOBAL")
    lazy val x = Name("x")
    lazy val y = Name("y")
    lazy val temp = Temp(42)
    lazy val prop = Prop(x, EStr("p"))
    lazy val propStr = Prop(x, EStr("!!"))
    lazy val propId = Prop(x, xExpr)

    // tests
    checkParseAndStringify("Ref", Ref)(
      global -> "@GLOBAL",
      x -> "x",
      temp -> "%42",
      prop -> "x.p",
      propStr -> "x[\"!!\"]",
      propId -> "x[x]",
    )

    // -------------------------------------------------------------------------
    // TODO types
    // -------------------------------------------------------------------------
    lazy val ty = Type(NumberT)
    checkParseAndStringify("Type", Type)(ty -> "Number")
  }

  init
}
