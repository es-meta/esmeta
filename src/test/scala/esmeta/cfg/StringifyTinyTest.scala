package esmeta.cfg

import esmeta.IR_TEST_DIR
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.BaseUtils.*
import esmeta.util.{Loc, Pos}
import esmeta.util.SystemUtils._
import scala.collection.mutable.ListBuffer

class StringifyTinyTest extends CFGTest {
  val name: String = "cfgStringifyTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // control flow graphs (CFGs)
    // -------------------------------------------------------------------------
    lazy val cfg = CFG(mainFunc, ListBuffer(mainFunc, func(1)))
    // tests
    checkStringify("CFG")(
      cfg -> """0: @main def f(x: T, y?: T) {
      |  0: let x = ~empty~ -> 1
      |  1: if x then 2 else 3
      |  2: {
      |    let x = ~empty~
      |    delete x.p
      |    return x
      |  }
      |  3: call %42 = x(x, y)
      |}
      |1: def f(x: T, y?: T) {
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
    lazy val mainFunc =
      Func(0, IRFunc.Head(true, IRFunc.Kind.AbsOp, "f", params), entry(0))
    def func(id: Int): Func =
      Func(id, IRFunc.Head(false, IRFunc.Kind.AbsOp, "f", params), entry(4))

    // tests
    checkStringify("Func")(
      mainFunc -> """0: @main def f(x: T, y?: T) {
      |  0: let x = ~empty~ -> 1
      |  1: if x then 2 else 3
      |  2: {
      |    let x = ~empty~
      |    delete x.p
      |    return x
      |  }
      |  3: call %42 = x(x, y)
      |}""".stripMargin,
      func(0) -> """0: def f(x: T, y?: T) {
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
    lazy val branch = Branch(0, Branch.Kind.If, xExpr)
    lazy val call = Call(0, temp, xExpr, List(xExpr, yExpr))

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
    checkStringify("Branch.Kind")(
      Branch.Kind.If -> "if",
      Branch.Kind.Loop("repeat") -> "loop[repeat]",
    )

    // -------------------------------------------------------------------------
    // IR elements
    // -------------------------------------------------------------------------
    lazy val params = List(xParam, yParam)
    lazy val xParam = IRFunc.Param(x, false, ty)
    lazy val yParam = IRFunc.Param(y, true, ty)
    lazy val let = ILet(x, empty)
    lazy val del = IDelete(prop)
    lazy val ret = IReturn(xExpr)
    lazy val xExpr = ERef(x)
    lazy val yExpr = ERef(y)
    lazy val empty = EConst("empty")
    lazy val prop = Prop(x, EStr("p"))
    lazy val x = Name("x")
    lazy val y = Name("y")
    lazy val temp = Temp(42)
    lazy val ty = Type("T")
  }

  init
}
