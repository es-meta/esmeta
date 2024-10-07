package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.lang.Syntax
import esmeta.ty.Ty
import esmeta.util.DoubleEquals
import scala.annotation.meta.field

// IR expressions
sealed trait Expr extends IRElem with LangEdge {
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
}
object Expr extends Parser.From(Parser.expr)
case class EParse(code: Expr, rule: Expr) extends Expr
case class EGrammarSymbol(name: String, params: List[Boolean]) extends Expr
case class ESourceText(expr: Expr) extends Expr
case class EYet(msg: String) extends Expr
case class EContains(list: Expr, expr: Expr) extends Expr
case class ESubstring(expr: Expr, from: Expr, to: Option[Expr]) extends Expr
case class ETrim(expr: Expr, isStarting: Boolean) extends Expr
case class ERef(ref: Ref) extends Expr
case class EUnary(uop: UOp, expr: Expr) extends Expr
case class EBinary(bop: BOp, left: Expr, right: Expr) extends Expr
case class EVariadic(vop: VOp, exprs: List[Expr]) extends Expr
case class EMathOp(mop: MOp, args: List[Expr]) extends Expr
case class EConvert(cop: COp, expr: Expr) extends Expr
case class EExists(ref: Ref) extends Expr
case class ETypeOf(base: Expr) extends Expr
case class EInstanceOf(base: Expr, target: Expr) extends Expr
case class ETypeCheck(base: Expr, ty: Type) extends Expr
case class ESizeOf(base: Expr) extends Expr
case class EClo(fname: String, captured: List[Name]) extends Expr
case class ECont(fname: String) extends Expr

// debugging expressions
case class EDebug(expr: Expr) extends Expr

// random number expressions
case class ERandom() extends Expr

// abstract syntax tree (AST) expressions
sealed trait AstExpr extends Expr {
  val name: String
}
case class ESyntactic(
  name: String,
  args: List[Boolean],
  rhsIdx: Int,
  children: List[Option[Expr]],
) extends AstExpr
case class ELexical(
  name: String,
  expr: Expr,
) extends AstExpr

// allocation expressions
sealed trait AllocExpr extends Expr { var asite: Int = -1 }
case class ERecord(tname: String, pairs: List[(String, Expr)]) extends AllocExpr
case class EMap(ty: (Type, Type), pairs: List[(Expr, Expr)]) extends AllocExpr
case class EList(exprs: List[Expr]) extends AllocExpr
case class ECopy(obj: Expr) extends AllocExpr
case class EKeys(map: Expr, intSorted: Boolean) extends AllocExpr

// literals
sealed trait LiteralExpr extends Expr
case class EMath(n: BigDecimal) extends LiteralExpr
case class EInfinity(pos: Boolean) extends LiteralExpr
case class ENumber(double: Double) extends LiteralExpr with DoubleEquals
case class EBigInt(bigInt: scala.math.BigInt) extends LiteralExpr
case class EStr(str: String) extends LiteralExpr
case class EBool(b: Boolean) extends LiteralExpr
case class EUndef() extends LiteralExpr
case class ENull() extends LiteralExpr
case class EEnum(name: String) extends LiteralExpr
case class ECodeUnit(c: Char) extends esmeta.ir.LiteralExpr
