package esmeta.cfg

import esmeta.CFG_TEST_DIR
import esmeta.cfg.*
import esmeta.util.BaseUtils.*
import esmeta.util.{Loc, Pos}
import esmeta.util.SystemUtils._
import scala.collection.mutable.ListBuffer

class ParseAndStringifyTinyTest extends CFGTest {
  val name: String = "cfgParseAndStringifyTest"

  // registration
  def init: Unit = {
    // check parser and stringifier from files
    for (file <- walkTree(CFG_TEST_DIR)) {
      val filename = file.getName
      if (cfgFilter(filename)) check(filename) {
        val name = file.toString
        cfgParseTestFile(name)
      }
    }

    // -------------------------------------------------------------------------
    // control flow graphs (CFGs)
    // -------------------------------------------------------------------------
    lazy val cfg = CFG(mainFunc, ListBuffer(mainFunc, func))
    // tests
    checkParseAndStringify("CFG", CFG)(
      cfg -> """0: @main f(x: T, y?: T) {
      |  0: let x = ~empty~ -> 1
      |  1: if x then 2 else 3
      |  2: {
      |    let x = ~empty~
      |    delete x.p
      |    return x
      |  }
      |  3: call %42 = x(x, y)
      |}
      |1: f(x: T, y?: T) {
      |  4: let x = ~empty~ -> 5
      |  5: if x then 6 else 7
      |  6: {
      |    let x = ~empty~
      |    delete x.p
      |    return x
      |  }
      |  7: call %42 = x(x, y)
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // functions
    // -------------------------------------------------------------------------
    def entry(start: Int) = {
      val blockSingle = Block(start + 0, ListBuffer(let))
      val branch = Branch(start + 1, Branch.Kind.If, xExpr)
      val block = Block(start + 2, ListBuffer(let, del, ret))
      val call = Call(start + 3, temp, xExpr, List(xExpr, yExpr))
      blockSingle.next = Some(branch)
      branch.thenNode = Some(block)
      branch.elseNode = Some(call)
      Some(blockSingle)
    }
    lazy val mainFunc = Func(0, true, Func.Kind.AbsOp, "f", params, entry(0))
    lazy val func = Func(1, false, Func.Kind.AbsOp, "f", params, entry(4))

    // tests
    checkParseAndStringify("Func", Func)(
      mainFunc -> """0: @main f(x: T, y?: T) {
      |  0: let x = ~empty~ -> 1
      |  1: if x then 2 else 3
      |  2: {
      |    let x = ~empty~
      |    delete x.p
      |    return x
      |  }
      |  3: call %42 = x(x, y)
      |}""".stripMargin,
      func -> """1: f(x: T, y?: T) {
      |  4: let x = ~empty~ -> 5
      |  5: if x then 6 else 7
      |  6: {
      |    let x = ~empty~
      |    delete x.p
      |    return x
      |  }
      |  7: call %42 = x(x, y)
      |}""".stripMargin,
    )
    checkParseAndStringify("Func.Kind", Func.Kind)(
      Func.Kind.AbsOp -> "",
      Func.Kind.NumMeth -> "<NUM>:",
      Func.Kind.SynDirOp -> "<SYNTAX>:",
      Func.Kind.ConcMeth -> "<CONC>:",
      Func.Kind.InternalMeth -> "<INTERNAL>:",
      Func.Kind.Builtin -> "<BUILTIN>:",
      Func.Kind.Clo -> "<CLO>:",
      Func.Kind.Cont -> "<CONT>:",
    )

    // -------------------------------------------------------------------------
    // parameters
    // -------------------------------------------------------------------------
    lazy val params = List(xParam, yParam)
    lazy val xParam = Func.Param(x, false, ty)
    lazy val yParam = Func.Param(y, true, ty)

    // tests
    checkParseAndStringify("Func.Param", Func.Param)(
      xParam -> "x: T",
      yParam -> "y?: T",
    )

    // -------------------------------------------------------------------------
    // nodes
    // -------------------------------------------------------------------------
    lazy val block = Block(0, ListBuffer(let, del, ret))
    lazy val branch = Branch(1, Branch.Kind.If, xExpr)
    lazy val call = Call(2, temp, xExpr, List(xExpr, yExpr))

    // tests
    checkParseAndStringify("Node", Node)(
      block -> """0: {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
      branch -> "1: if x",
      call -> "2: call %42 = x(x, y)",
    )
    checkParseAndStringify("Branch.Kind", Branch.Kind)(
      Branch.Kind.If -> "if",
      Branch.Kind.Loop("repeat") -> "loop[repeat]",
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
    lazy val parse = EParse(xExpr, rule)
    lazy val rule = EParseRule("A", List(true, false))
    lazy val yet = EYet("NOT YET")
    lazy val contains = EContains(xExpr, xExpr)
    lazy val xExpr = ERef(x)
    lazy val yExpr = ERef(y)
    lazy val unary = EUnary(UOp.Neg, xExpr)
    lazy val binary = EBinary(BOp.Plus, xExpr, xExpr)
    lazy val convert = EConvert(COp.ToBigInt, xExpr)
    lazy val typeOf = ETypeOf(xExpr)
    lazy val typeCheck = ETypeCheck(xExpr, ty)
    // AST expressions
    lazy val ast = ESyntactic("Identifier", Nil, 1, 2, Nil)
    lazy val astArgs = ESyntactic("Identifier", List(true, false), 1, 2, Nil)
    lazy val astSingle =
      ESyntactic("Identifier", List(true, false), 1, 2, List(xExpr))
    lazy val astMultiple =
      ESyntactic("Identifier", List(true, false), 1, 2, List(xExpr, yExpr))
    lazy val lex = ELexical("Identifier", xExpr)
    // allocation expressions
    lazy val rec = EMap("T", List(EUndef -> EBool(true), ENull -> EAbsent), 42)
    lazy val list = EList(List(EUndef, ENull, EAbsent), 42)
    lazy val symbol = ESymbol(ENull, 42)
    lazy val copy = ECopy(xExpr, 42)
    lazy val keys = EKeys(xExpr, false, 42)
    lazy val keysInt = EKeys(xExpr, true, 42)
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
      parse -> "(parse x (rule |A|[TF]))",
      rule -> "(rule |A|[TF])",
      yet -> "(yet \"NOT YET\")",
      contains -> "(contains x x)",
      xExpr -> "x",
      unary -> "(- x)",
      binary -> "(+ x x)",
      convert -> "([bigint] x)",
      typeOf -> "(typeof x)",
      typeCheck -> "(? x: T)",
      // AST expressions
      ast -> "|Identifier|<1, 2>",
      astArgs -> "|Identifier|[TF]<1, 2>",
      astSingle -> "|Identifier|[TF]<1, 2>(x)",
      astMultiple -> "|Identifier|[TF]<1, 2>(x, y)",
      lex -> "|Identifier|(x)",
      // allocation expressions
      rec -> "(new T(undefined -> true, null -> absent))[#42]",
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
    lazy val ty = Type("T")
    checkParseAndStringify("Type", Type)(ty -> "T")
  }

  init
}
