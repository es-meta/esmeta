package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.lang.Syntax

// IR instructions
sealed trait Inst extends IRElem with LangEdge:
  // conversion to instruction lists
  def toList: List[Inst] = this match
    case ISeq(is) => is
    case i        => List(i)

  var comment: Option[String] = None
  def addCmt(comment: String): this.type =
    this.comment = Some(comment)
    this

  def setCmt(comment: Option[String]): this.type =
    this.comment = comment
    this

  def passCmt(from: Inst): this.type =
    this.comment = from.comment
    this
object Inst extends Parser.From(Parser.inst)

// normal instructions
sealed trait NormalInst extends Inst
case class IExpr(expr: Expr) extends NormalInst
case class ILet(lhs: Name, expr: Expr) extends NormalInst
case class IAssign(ref: Ref, expr: Expr) extends NormalInst
case class IExpand(base: Ref, expr: Expr) extends NormalInst
case class IDelete(base: Ref, expr: Expr) extends NormalInst
case class IPush(elem: Expr, list: Expr, front: Boolean) extends NormalInst
case class IPop(lhs: Local, list: Expr, front: Boolean) extends NormalInst
case class IReturn(expr: Expr) extends NormalInst with Return
case class IAssert(expr: Expr) extends NormalInst
case class IPrint(expr: Expr) extends NormalInst
case class INop() extends NormalInst
object NormalInsts extends Parser.From(Parser.normalInsts)

// branch instructions
sealed trait BranchInst extends Inst
case class IIf(cond: Expr, thenInst: Inst, elseInst: Inst) extends BranchInst
case class IWhile(cond: Expr, body: Inst) extends BranchInst
object BranchInst extends Parser.From(Parser.branchInst)

// call instructions
sealed trait CallInst extends Inst { val lhs: Local }
case class ICall(lhs: Local, fexpr: Expr, args: List[Expr]) extends CallInst
case class ISdoCall(lhs: Local, base: Expr, op: String, args: List[Expr])
  extends CallInst

// special instructions
case class ISeq(insts: List[Inst]) extends Inst
