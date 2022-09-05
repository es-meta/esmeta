package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
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
    lazy val npEmptyView = NodePoint(mainFunc, block, View())
    lazy val rp = ReturnPoint(mainFunc, irView)
    lazy val rpEmptyView = ReturnPoint(mainFunc, View())

    checkStringify("ControlPoint")(
      np -> "f[0]:Block[0]:[call: 0, 0, 0][loop: 0(3), 0(3)]",
      npEmptyView -> "f[0]:Block[0]",
      rp -> "f[0]:RETURN:[call: 0, 0, 0][loop: 0(3), 0(3)]",
      rpEmptyView -> "f[0]:RETURN",
    )

    // -------------------------------------------------------------------------
    // CFG elements
    // -------------------------------------------------------------------------
    def entry(start: Int) = {
      val blockSingle = Block(start + 0, ListBuffer(let))
      val branch = Branch(start + 1, BranchKind.If, xExpr)
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
    lazy val branch = Branch(0, BranchKind.Loop("repeat"), xExpr)
    lazy val call = Call(0, callInst)

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
    lazy val ty = Type(NameT("T"))
  }

  init
}
