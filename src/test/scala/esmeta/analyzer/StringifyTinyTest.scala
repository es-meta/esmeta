package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.ty.*
import scala.collection.mutable.ListBuffer

class StringifyTinyTest extends AnalyzerTest {
  import AnalyzerTest.*, analyzer.*

  val name: String = "analyzerStringifyTest"

  // registration
  def init: Unit = {
    // TODO
    // // -------------------------------------------------------------------------
    // // Views
    // // -------------------------------------------------------------------------
    // lazy val insensView = View()
    // lazy val loopCtxt = LoopCtxt(branch, 3)
    // lazy val irView = View(
    //   calls = List(call, call, call),
    //   loops = List(loopCtxt, loopCtxt),
    // )
    // checkStringify("View")(
    //   View() -> "",
    //   View(calls = List(call, call)) -> "[call: 0, 0]",
    //   View(loops = List(loopCtxt)) -> "[loop: 0(3)]",
    //   irView -> "[call: 0, 0, 0][loop: 0(3), 0(3)]",
    // )

    // // -------------------------------------------------------------------------
    // // Analysis Points
    // // -------------------------------------------------------------------------
    // lazy val callPoint = CallPoint(NodePoint(mainFunc, call, View()), mainFunc)
    // lazy val argAssignPoint = ArgAssignPoint(callPoint, 0)
    // lazy val internalReturnPoint = InternalReturnPoint(nodePoint, ret)
    // lazy val fieldBasePoint = FieldBasePoint(fieldPoint)
    // lazy val fieldPoint = FieldPoint(nodePoint, field)
    // lazy val unaryOpPoint = UnaryOpPoint(nodePoint, unary)
    // lazy val binaryOpPoint = BinaryOpPoint(nodePoint, binary)
    // lazy val nodePoint = NodePoint(mainFunc, block, View())
    // lazy val nodePointWithView = NodePoint(mainFunc, block, irView)
    // lazy val returnPoint = ReturnPoint(mainFunc, View())
    // lazy val returnPointWithView = ReturnPoint(mainFunc, irView)

    // checkStringify("AnalysisPoint")(
    //   callPoint -> "function call from f to f",
    //   argAssignPoint -> "argument assignment to first parameter _x_ when function call from f to f",
    //   internalReturnPoint -> "return statement in f",
    //   fieldBasePoint -> "base infield lookup in f",
    //   fieldPoint -> "field lookup in f",
    //   unaryOpPoint -> "unary operation (-) in f",
    //   binaryOpPoint -> "binary operation (+) in f",
    //   nodePoint -> "f[0]:Block[0]",
    //   nodePointWithView -> "f[0]:Block[0]:[call: 0, 0, 0][loop: 0(3), 0(3)]",
    //   returnPoint -> "f[0]:RETURN",
    //   returnPointWithView -> "f[0]:RETURN:[call: 0, 0, 0][loop: 0(3), 0(3)]",
    // )

    // // -------------------------------------------------------------------------
    // // Type Errors
    // // -------------------------------------------------------------------------
    // lazy val paramTypeMismatch = ParamTypeMismatch(argAssignPoint, ty)
    // lazy val returnTypeMismatch = ReturnTypeMismatch(internalReturnPoint, ty)
    // lazy val arityMismatch = ArityMismatch(callPoint, 3)
    // lazy val uncheckedAbruptError = UncheckedAbruptError(riaPoint, ty)
    // lazy val invalidBaseError = InvalidBaseError(fieldBasePoint, ty)
    // lazy val unaryOpTypeMismatch = UnaryOpTypeMismatch(unaryOpPoint, ty)
    // lazy val binaryOpTypeMismatch = BinaryOpTypeMismatch(binaryOpPoint, ty, ty)

    // checkStringify("TypeError")(
    //   paramTypeMismatch ->
    //   """[ParamTypeMismatch] argument assignment to first parameter _x_ when function call from f to f
    //     |- expected: T
    //     |- actual  : T""".stripMargin,
    //   returnTypeMismatch ->
    //   """[ReturnTypeMismatch] return statement in f
    //     |- expected: T
    //     |- actual  : T""".stripMargin,
    //   arityMismatch ->
    //   """[ArityMismatch] function call from f to f
    //     |- expected: [1, 2]
    //     |- actual  : 3""".stripMargin,
    //   invalidBaseError ->
    //   """[InvalidBaseError] base infield lookup in f
    //     |- base    : T""".stripMargin,
    //   unaryOpTypeMismatch ->
    //   """[UnaryOpTypeMismatch] unary operation (-) in f
    //     |- operand : T""".stripMargin,
    //   binaryOpTypeMismatch ->
    //   """[BinaryOpTypeMismatch] binary operation (+) in f
    //     |- left    : T
    //     |- right   : T""".stripMargin,
    // )

    // // -------------------------------------------------------------------------
    // // CFG elements
    // // -------------------------------------------------------------------------
    // def entry(start: Int): Node = {
    //   val blockSingle = Block(start + 0, ListBuffer(let))
    //   val branch = Branch(start + 1, BranchKind.If, xExpr)
    //   val block = Block(start + 2, ListBuffer(let, del, ret))
    //   val call = Call(start + 3, ICall(temp, xExpr, List(xExpr, yExpr)))
    //   blockSingle.next = Some(branch)
    //   branch.thenNode = Some(block)
    //   branch.elseNode = Some(call)
    //   blockSingle
    // }
    // lazy val mainFunc =
    //   Func(0, irMainFunc, entry(0))
    // def func(id: Int): Func =
    //   Func(id, irFunc, entry(4))
    // lazy val block = Block(0, ListBuffer(let, del, ret))
    // lazy val branch = Branch(0, BranchKind.While, xExpr)
    // lazy val call = Call(0, callInst)

    // // -------------------------------------------------------------------------
    // // IR elements
    // // -------------------------------------------------------------------------
    // lazy val irMainFunc = IRFunc(true, IRFuncKind.AbsOp, "f", params, irTy, seq)
    // lazy val irFunc = IRFunc(false, IRFuncKind.AbsOp, "f", params, irTy, seq)
    // lazy val seq = ISeq(List(let, del, ret))
    // lazy val params = List(xParam, yParam)
    // lazy val xParam = Param(x, irTy, false)
    // lazy val yParam = Param(y, irTy, true)
    // lazy val let = ILet(x, empty)
    // lazy val del = IDelete(field)
    // lazy val ret = IReturn(xExpr)
    // lazy val unary = EUnary(UOp.Neg, xExpr)
    // lazy val binary = EBinary(BOp.Add, xExpr, xExpr)
    // lazy val callInst = ICall(temp, xExpr, List(xExpr, yExpr))
    // lazy val xExpr = ERef(x)
    // lazy val yExpr = ERef(y)
    // lazy val empty = EEnum("empty")
    // lazy val field = Field(x, EStr("p"))
    // lazy val x = Name("x")
    // lazy val y = Name("y")
    // lazy val temp = Temp(42)
    // lazy val ty = RecordT("T")
    // lazy val irTy = Type(ty)
  }

  init
}
