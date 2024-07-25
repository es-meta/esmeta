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
    import IRTest.*

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
    checkParseAndStringify("Func", Func)(
      mainFunc -> """@main def f(
      |  x: Number,
      |  y?: Number,
      |): Number = {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
      func -> """def f(
      |  x: Number,
      |  y?: Number,
      |): Number = {
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
    checkParseAndStringify("Param", Param)(
      xParam -> "x: Number",
      yParam -> "y?: Number",
    )

    // -------------------------------------------------------------------------
    // instructions
    // -------------------------------------------------------------------------
    checkParseAndStringify("Inst", Inst)(
      xExprInst -> "x",
      let -> "let x = ~empty~",
      del -> "delete x.p",
      pushFront -> "push x > y",
      pushBack -> "push y < x",
      ret -> "return x",
      assertInst -> "assert x",
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
      |}""".stripMargin,
      ifElseInst -> """if x {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |} else {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
      whileInst -> """while x {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
      call -> "call %42 = x(x, y)",
      sdoCall -> "sdo-call %42 = x->Evaluation(x, y)",
    )

    // -------------------------------------------------------------------------
    // expressions
    // -------------------------------------------------------------------------
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
      substring -> "(substring x x)",
      substringTo -> "(substring x x x)",
      trim -> "(trim (trim > x) <)",
      trimStart -> "(trim > x)",
      trimEnd -> "(trim x <)",
      xExpr -> "x",
      unary -> "(- x)",
      binary -> "(+ x x)",
      variadic -> "(min x x x)",
      mathOp -> "([math:tan] x)",
      clamp -> "(clamp x x x)",
      convert -> "([bigInt] x)",
      typeOf -> "(typeof x)",
      typeCheck -> "(? x: \"Number\")",
      // debugging expressions
      debug -> "(debug x)",
      // random number expressions
      rand -> "(random)",
      // AST expressions
      ast -> "|Identifier|<1>",
      astArgs -> "|Identifier|[TF]<1>",
      astSingle -> "|Identifier|[TF]<1>(x)",
      astMultiple -> "|Identifier|[TF]<1>(x, y)",
      astComplex -> "|Identifier|[TF]<3>(, x, , y)",
      lex -> "|Identifier|(x)",
      // allocation expressions
      rec -> """(new T { "A" : true, "B" : absent })""",
      list -> "(new [undefined, null, absent])",
      symbol -> "(new 'null)",
      copy -> "(copy x)",
      keys -> "(keys x)",
      keysInt -> "(keys-int x)",
      getChildren -> "(get-children x)",
      getItems -> "(get-items x x)",
      // allocation expressions with allocation sites
      recASite -> """(new T { "A" : true, "B" : absent })[#3]""".stripMargin,
      listASite -> "(new [undefined, null, absent])[#1]",
      symbolASite -> "(new 'null)[#7]",
      copyASite -> "(copy x)[#42]",
      keysASite -> "(keys x)[#5]",
      keysIntASite -> "(keys-int x)[#6]",
      getChildrenASite -> "(get-children x)[#9]",
      getItemsASite -> "(get-items x x)[#10]",
      // literals
      EMath(4) -> "4",
      EInfinity(true) -> "+INF",
      EInfinity(false) -> "-INF",
      ENumber(3.0) -> "3.0f",
      ENumber(Double.PositiveInfinity) -> "+NUM_INF",
      ENumber(Double.NegativeInfinity) -> "-NUM_INF",
      ENumber(Double.NaN) -> "NaN",
      EBigInt(1024) -> "1024n",
      EStr("hi") -> "\"hi\"",
      EBool(true) -> "true",
      EBool(false) -> "false",
      EUndef() -> "undefined",
      ENull() -> "null",
      EAbsent() -> "absent",
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
    checkParseAndStringify("Ref", Ref)(
      global -> "@GLOBAL",
      x -> "x",
      temp -> "%42",
      field -> "x.p",
      fieldStr -> "x[\"!!\"]",
      fieldId -> "x[x]",
    )

    // -------------------------------------------------------------------------
    // types
    // -------------------------------------------------------------------------
    checkParseAndStringify("Type", Type)(ty -> "Number")
  }

  init
}
