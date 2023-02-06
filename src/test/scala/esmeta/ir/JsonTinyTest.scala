package esmeta.ir

import esmeta.ir.util.JsonProtocol.given
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*

/** JSON test */
class JsonTinyTest extends IRTest {
  val name: String = "irJsonTest"

  // registration
  def init: Unit = {
    import IRTest.*

    // -------------------------------------------------------------------------
    // functions
    // -------------------------------------------------------------------------
    checkJsonWithString("Func")(
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
    checkJsonWithString("FuncKind")(
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
    checkJsonWithString("Param")(
      xParam -> "x: Number",
      yParam -> "y?: Number",
    )

    // -------------------------------------------------------------------------
    // instructions
    // -------------------------------------------------------------------------
    checkJsonWithString("Inst")(
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
    checkJsonWithString("Expr")(
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
      rec -> "(new T(undefined -> true, null -> absent))",
      list -> "(new [undefined, null, absent])",
      symbol -> "(new 'null)",
      copy -> "(copy x)",
      keys -> "(keys x)",
      keysInt -> "(keys-int x)",
      getChildren -> "(get-children x)",
      getItems -> "(get-items x x)",
      // allocation expressions with allocation sites
      recASite -> "(new T(undefined -> true, null -> absent))[#3]",
      listASite -> "(new [undefined, null, absent])[#1]",
      symbolASite -> "(new 'null)[#7]",
      copyASite -> "(copy x)[#42]",
      keysASite -> "(keys x)[#5]",
      keysIntASite -> "(keys-int x)[#6]",
      getChildrenASite -> "(get-children x)[#9]",
      getItemsASite -> "(get-items x x)[#10]",
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
    checkJsonWithString("UOp")(
      UOp.Neg -> "-",
      UOp.Not -> "!",
      UOp.BNot -> "~",
    )
    checkJsonWithString("BOp")(
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
    checkJsonWithString("VOp")(
      VOp.Min -> "min",
      VOp.Max -> "max",
      VOp.Concat -> "concat",
    )
    checkJsonWithString("MOp")(
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
    checkJsonWithString("COp")(
      COp.ToApproxNumber -> "[approx-number]",
      COp.ToNumber -> "[number]",
      COp.ToBigInt -> "[bigInt]",
      COp.ToStr(None) -> "[str]",
      COp.ToStr(Some(xExpr)) -> "[str x]",
    )

    // -------------------------------------------------------------------------
    // references
    // -------------------------------------------------------------------------
    checkJsonWithString("Ref")(
      global -> "@GLOBAL",
      x -> "x",
      temp -> "%42",
      prop -> "x.p",
      propStr -> "x[\"!!\"]",
      propId -> "x[x]",
    )

    // -------------------------------------------------------------------------
    // types
    // -------------------------------------------------------------------------
    checkJsonWithString("Type")(ty -> "Number")
  }

  init
}
