package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.util.{DoubleEquals, Locational}

// CFG expressions
sealed trait Expr extends CFGElem with Locational
object Expr extends Parser.From[Expr]
case class EComp(tyExpr: Expr, valExpr: Expr, tgtExpr: Expr) extends Expr
case class EIsCompletion(expr: Expr) extends Expr
case class EReturnIfAbrupt(expr: Expr, check: Boolean) extends Expr
case class EPop(list: Expr, front: Boolean) extends Expr
case class EParse(code: Expr, rule: Expr) extends Expr
case class EParseRule(name: String, params: List[Boolean]) extends Expr
case class ESourceText(expr: Expr) extends Expr
case class EYet(msg: String) extends Expr
case class EContains(list: Expr, elem: Expr) extends Expr
case class EStrConcat(exprs: List[Expr]) extends Expr
case class ESubstring(expr: Expr, from: Expr, to: Expr) extends Expr
case class ERef(ref: Ref) extends Expr
case class EUnary(uop: UOp, expr: Expr) extends Expr
case class EBinary(bop: BOp, left: Expr, right: Expr) extends Expr
case class EVariadic(vop: VOp, exprs: List[Expr]) extends Expr
case class EConvert(cop: COp, expr: Expr) extends Expr
case class ETypeOf(base: Expr) extends Expr
case class ETypeCheck(base: Expr, ty: Type) extends Expr
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
  bits: Int,
  children: List[Expr],
) extends AstExpr
case class ELexical(
  name: String,
  expr: Expr,
) extends AstExpr

// allocation expressions
sealed trait AllocExpr extends Expr { val asite: Int }
case class EMap(tname: String, props: List[(Expr, Expr)], asite: Int)
  extends AllocExpr
case class EList(exprs: List[Expr], asite: Int) extends AllocExpr
case class EListConcat(exprs: List[Expr], asite: Int) extends AllocExpr
case class ESymbol(desc: Expr, asite: Int) extends AllocExpr
case class ECopy(obj: Expr, asite: Int) extends AllocExpr
case class EKeys(map: Expr, intSorted: Boolean, asite: Int) extends AllocExpr

// literals
sealed trait Literal extends Expr
case class EMathVal(n: BigDecimal) extends Literal
case class ENumber(n: Double) extends Literal with DoubleEquals(n)
case class EBigInt(n: scala.math.BigInt) extends Literal
case class EStr(str: String) extends Literal
case class EBool(b: Boolean) extends Literal
case object EUndef extends Literal
case object ENull extends Literal
case object EAbsent extends Literal
case class EConst(name: String) extends Literal
