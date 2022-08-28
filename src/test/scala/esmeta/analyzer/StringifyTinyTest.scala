package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ty.*
import scala.collection.mutable.ListBuffer

class StringifyTinyTest extends AnalyzerTest {
  val name: String = "analyzerStringifyTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // Views
    // -------------------------------------------------------------------------
    lazy val insensView = View()
    lazy val loopCtxt = LoopCtxt(branch, 3)
    lazy val irView = View(
      calls = List(call, call, call),
      loops = List(loopCtxt, loopCtxt),
    )
    checkStringify("View")(
      insensView -> "[call: ][loop: ]",
      irView -> "[call: 0, 0, 0][loop: 0(3), 0(3)]",
    )

    // -------------------------------------------------------------------------
    // Control Points
    // -------------------------------------------------------------------------
    lazy val np = NodePoint(mainFunc, block, irView)
    lazy val rp = ReturnPoint(mainFunc, irView)

    checkStringify("ControlPoint")(
      np -> "[call: 0, 0, 0][loop: 0(3), 0(3)]:Block[0]",
      rp -> "[call: 0, 0, 0][loop: 0(3), 0(3)]:RET:f[0]",
    )

    // -------------------------------------------------------------------------
    // CFG elements
    // -------------------------------------------------------------------------
    def entry(start: Int) = {
      val blockSingle = Block(start + 0, ListBuffer(let))
      val branch = Branch(start + 1, Branch.Kind.If, xExpr)
      val block = Block(start + 2, ListBuffer(let, del, ret))
      val call = Call(start + 3, ICall(temp, xExpr, List(xExpr, yExpr)))
      blockSingle.next = Some(branch)
      branch.thenNode = Some(block)
      branch.elseNode = Some(call)
      Some(blockSingle)
    }
    lazy val mainFunc =
      Func(0, irMainFunc, entry(0))
    def func(id: Int): Func =
      Func(id, irFunc, entry(4))
    lazy val block = Block(0, ListBuffer(let, del, ret))
    lazy val branch = Branch(0, Branch.Kind.Loop("repeat"), xExpr)
    lazy val call = Call(0, callInst)

    // -------------------------------------------------------------------------
    // IR elements
    // -------------------------------------------------------------------------
    lazy val irMainFunc = IRFunc(true, IRFunc.Kind.AbsOp, "f", params, ty, seq)
    lazy val irFunc = IRFunc(false, IRFunc.Kind.AbsOp, "f", params, ty, seq)
    lazy val seq = ISeq(List(let, del, ret))
    lazy val params = List(xParam, yParam)
    lazy val xParam = IRFunc.Param(x, false, ty)
    lazy val yParam = IRFunc.Param(y, true, ty)
    lazy val let = ILet(x, empty)
    lazy val del = IDelete(prop)
    lazy val ret = IReturn(xExpr)
    lazy val callInst = ICall(temp, xExpr, List(xExpr, yExpr))
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
