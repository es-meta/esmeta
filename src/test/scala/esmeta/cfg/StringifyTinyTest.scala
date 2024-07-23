package esmeta.cfg

import esmeta.IR_TEST_DIR
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.{Loc, Pos}
import esmeta.util.SystemUtils._
import scala.collection.mutable.ListBuffer

/** stringify test */
class StringifyTinyTest extends CFGTest {
  val name: String = "cfgStringifyTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // control flow graphs (CFGs)
    // -------------------------------------------------------------------------
    lazy val cfg = CFG(List(mainFunc, func(1)))
    // tests
    checkStringify("CFG")(
      cfg -> """0: @main def f(x: Number, y?: Number): Number {
      |  0: let x = ~empty~ -> 1
      |  1: if x then 2 else 3
      |  2: {
      |    let x = ~empty~
      |    delete x.p
      |    return x
      |  }
      |  3: call %42 = x(x, y)
      |}
      |1: def f(x: Number, y?: Number): Number {
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
    def entry(start: Int): Node = {
      val blockSingle = Block(start + 0, ListBuffer(let))
      val branch = Branch(start + 1, BranchKind.If, xExpr)
      val block = Block(start + 2, ListBuffer(let, del, ret))
      val call = Call(start + 3, ICall(temp, xExpr, List(xExpr, yExpr)))
      blockSingle.next = Some(branch)
      branch.thenNode = Some(block)
      branch.elseNode = Some(call)
      blockSingle
    }
    lazy val mainFunc =
      Func(0, irMainFunc, entry(0))
    def func(id: Int): Func =
      Func(id, irFunc, entry(4))

    // tests
    checkStringify("Func")(
      mainFunc -> """0: @main def f(x: Number, y?: Number): Number {
      |  0: let x = ~empty~ -> 1
      |  1: if x then 2 else 3
      |  2: {
      |    let x = ~empty~
      |    delete x.p
      |    return x
      |  }
      |  3: call %42 = x(x, y)
      |}""".stripMargin,
      func(0) -> """0: def f(x: Number, y?: Number): Number {
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
    // nodes
    // -------------------------------------------------------------------------
    lazy val block = Block(0, ListBuffer(let, del, ret))
    lazy val branch = Branch(0, BranchKind.If, xExpr)
    lazy val call = Call(0, callInst)

    // tests
    checkStringify("Node")(
      block -> """0: {
      |  let x = ~empty~
      |  delete x.p
      |  return x
      |}""".stripMargin,
      branch -> "0: if x",
      call -> "0: call %42 = x(x, y)",
    )
    checkStringify("BranchKind")(
      BranchKind.If -> "if",
      BranchKind.While -> "while",
    )

    // -------------------------------------------------------------------------
    // IR elements
    // -------------------------------------------------------------------------
    lazy val irMainFunc = IRFunc(true, IRFuncKind.AbsOp, "f", params, ty, seq)
    lazy val irFunc = IRFunc(false, IRFuncKind.AbsOp, "f", params, ty, seq)
    lazy val seq = ISeq(List(let, del, ret))
    lazy val params = List(xParam, yParam)
    lazy val xParam = Param(x, ty, false)
    lazy val yParam = Param(y, ty, true)
    lazy val let = ILet(x, empty)
    lazy val del = IDelete(field)
    lazy val ret = IReturn(xExpr)
    lazy val callInst = ICall(temp, xExpr, List(xExpr, yExpr))
    lazy val xExpr = ERef(x)
    lazy val yExpr = ERef(y)
    lazy val empty = EEnum("empty")
    lazy val field = Field(x, EStr("p"))
    lazy val x = Name("x")
    lazy val y = Name("y")
    lazy val temp = Temp(42)
    lazy val ty = Type(NumberT)
  }

  init
}
