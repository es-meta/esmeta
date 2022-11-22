package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.lang.Syntax
import esmeta.util.DoubleEquals

// IR expressions
sealed trait Expr extends IRElem with LangEdge
object Expr extends Parser.From(Parser.expr)
case class EComp(tyExpr: Expr, valExpr: Expr, tgtExpr: Expr) extends Expr
case class EIsCompletion(expr: Expr) extends Expr
case class EReturnIfAbrupt(expr: Expr, check: Boolean) extends Expr with Return
case class EPop(list: Expr, front: Boolean) extends Expr
case class EParse(code: Expr, rule: Expr) extends Expr
case class ENt(name: String, params: List[Boolean]) extends Expr
case class ESourceText(expr: Expr) extends Expr
case class EYet(msg: String) extends Expr
case class EContains(list: Expr, expr: Expr, field: Option[(Type, String)])
  extends Expr
case class ESubstring(expr: Expr, from: Expr, to: Option[Expr]) extends Expr
case class ERef(ref: Ref) extends Expr
case class EUnary(uop: UOp, expr: Expr) extends Expr
case class EBinary(bop: BOp, left: Expr, right: Expr) extends Expr
case class EVariadic(vop: VOp, exprs: List[Expr]) extends Expr
case class EClamp(target: Expr, lower: Expr, upper: Expr) extends Expr
case class EMathOp(mop: MOp, args: List[Expr]) extends Expr
case class EConvert(cop: COp, expr: Expr) extends Expr
case class ETypeOf(base: Expr) extends Expr
case class ETypeCheck(base: Expr, tyExpr: Expr) extends Expr
case class EDuplicated(list: Expr) extends Expr
case class EIsArrayIndex(expr: Expr) extends Expr
case class EClo(fname: String, captured: List[Name]) extends Expr
case class ECont(fname: String) extends Expr

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
case class EMap(tname: String, props: List[(Expr, Expr)]) extends AllocExpr
case class EList(exprs: List[Expr]) extends AllocExpr
case class EListConcat(exprs: List[Expr]) extends AllocExpr
case class ESymbol(desc: Expr) extends AllocExpr
case class ECopy(obj: Expr) extends AllocExpr
case class EKeys(map: Expr, intSorted: Boolean) extends AllocExpr
case class EGetChildren(kindOpt: Option[Expr], ast: Expr) extends AllocExpr

// literals
sealed trait LiteralExpr extends Expr
case class EMathVal(n: BigDecimal) extends LiteralExpr
case class ENumber(n: Double) extends LiteralExpr with DoubleEquals(n)
case class EBigInt(n: scala.math.BigInt) extends LiteralExpr
case class EStr(str: String) extends LiteralExpr
case class EBool(b: Boolean) extends LiteralExpr
case class EUndef() extends LiteralExpr
case class ENull() extends LiteralExpr
case class EAbsent() extends LiteralExpr
case class EConst(name: String) extends LiteralExpr
case class ECodeUnit(c: Char) extends esmeta.ir.LiteralExpr
