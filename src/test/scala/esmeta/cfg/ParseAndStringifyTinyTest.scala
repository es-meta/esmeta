package esmeta.cfg

import esmeta.util.BaseUtils.*
import esmeta.cfg.*

class ParseAndStringifyTinyTest extends CFGTest {
  val name: String = "cfgParseAndStringifyTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // control flow graphs (CFGs)
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("CFG", CFG)( /* TODO */ )

    // -------------------------------------------------------------------------
    // functions
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Func", Func)( /* TODO */ )

    // -------------------------------------------------------------------------
    // parameters
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Param", Param)( /* TODO */ )

    // -------------------------------------------------------------------------
    // nodes
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Node", Node)( /* TODO */ )

    // -------------------------------------------------------------------------
    // instructions
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Inst", Inst)( /* TODO */ )

    // -------------------------------------------------------------------------
    // expressions
    // -------------------------------------------------------------------------
    lazy val comp = EComp(normal, empty, xExpr)
    lazy val isComp = EIsCompletion(xExpr)
    lazy val riaCheck = EReturnIfAbrupt(xExpr, true)
    lazy val riaNoCheck = EReturnIfAbrupt(xExpr, false)
    lazy val popFront = EPop(xExpr, true)
    lazy val popBack = EPop(xExpr, false)
    lazy val yet = EYet("NOT YET")
    lazy val contains = EContains(xExpr, xExpr)
    lazy val xExpr = ERef(x)
    lazy val unary = EUnary(UOp.Neg, xExpr)
    lazy val binary = EBinary(BOp.Plus, xExpr, xExpr)
    lazy val convert = EConvert(COp.ToBigInt, xExpr)
    lazy val typeOf = ETypeOf(xExpr)
    lazy val typeCheck = ETypeCheck(xExpr, ty)
    lazy val map = EMap("T", List(EUndef -> EBool(true), ENull -> EAbsent), 42)
    lazy val list = EList(List(EUndef, ENull, EAbsent), 42)
    lazy val symbol = ESymbol(ENull, 42)
    lazy val copy = ECopy(xExpr, 42)
    lazy val keys = EKeys(xExpr, false, 42)
    lazy val keysInt = EKeys(xExpr, true, 42)
    lazy val normal = EConst("normal")
    lazy val empty = EConst("empty")
    lazy val clo = EClo(42, Nil)
    lazy val cloWithCaptured = EClo(42, List(x))

    // tests
    checkParseAndStringify("Expr", Expr)(
      comp -> "comp[~normal~/~empty~](x)",
      isComp -> "(comp? x)",
      riaCheck -> "[? x]",
      riaNoCheck -> "[! x]",
      popFront -> "(pop < x)",
      popBack -> "(pop > x)",
      yet -> "(yet \"NOT YET\")",
      contains -> "(contains x x)",
      xExpr -> "x",
      unary -> "(- x)",
      binary -> "(+ x x)",
      convert -> "([bigint] x)",
      typeOf -> "(typeof x)",
      typeCheck -> "(? x: T)",
      // allocation expressions
      map -> "(new T(undefined -> true, null -> absent))[#42]",
      list -> "(new [undefined, null, absent])[#42]",
      symbol -> "(new 'null)[#42]",
      copy -> "(copy x)[#42]",
      keys -> "(keys x)[#42]",
      keysInt -> "(keys-int x)[#42]",
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
      clo -> "clo[42]",
      cloWithCaptured -> "clo[42](x)",
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
      COp.ToBigInt -> "[bigint]",
      COp.ToNumber -> "[number]",
      COp.ToStr(None) -> "[str]",
      COp.ToStr(Some(xExpr)) -> "[str x]",
    )

    // -------------------------------------------------------------------------
    // references
    // -------------------------------------------------------------------------
    lazy val global = Global("GLOBAL")
    lazy val x = Local("x")
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
    lazy val ty = Type("T")
    checkParseAndStringify("Type", Type)(ty -> "T")
  }

  init
}
