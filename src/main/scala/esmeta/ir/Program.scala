package esmeta.ir

import esmeta.util.BaseUtils.*
import esmeta.util.DoubleEquals
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

// -----------------------------------------------------------------------------
// IR Programs
// -----------------------------------------------------------------------------
case class Program(insts: List[Inst]) extends IRElem
object Program extends Parser[Program]

// -----------------------------------------------------------------------------
// IR Instruction
// -----------------------------------------------------------------------------
sealed trait Inst extends IRElem
object Insts extends Parser[List[Inst]]
object Inst extends Parser[Inst]

/** normal instructions */
sealed trait NormalInst extends Inst
case class IExpr(expr: Expr) extends NormalInst
case class ILet(id: Id, expr: Expr) extends NormalInst
case class IAssign(ref: Ref, expr: Expr) extends NormalInst
case class IDelete(ref: Ref) extends NormalInst
case class IAppend(expr: Expr, list: Expr) extends NormalInst
case class IPrepend(expr: Expr, list: Expr) extends NormalInst
case class IReturn(expr: Expr) extends NormalInst
case class IThrow(name: String) extends NormalInst
case class IAssert(expr: Expr) extends NormalInst
case class IPrint(expr: Expr) extends NormalInst

/** conditional instructions */
sealed trait CondInst extends Inst
case class IIf(cond: Expr, thenInst: Inst, elseInst: Inst) extends CondInst
case class IWhile(cond: Expr, body: Inst) extends CondInst

/** call instructions */
sealed trait CallInst extends Inst
case class IApp(id: Id, fexpr: Expr, args: List[Expr]) extends CallInst

/** arrow instructions */
sealed trait ArrowInst extends Inst
case class IWithCont(id: Id, params: List[Id], body: Inst) extends ArrowInst

/** sequence instructions */
case class ISeq(insts: List[Inst]) extends Inst

// -----------------------------------------------------------------------------
// IR Expressions
// -----------------------------------------------------------------------------
sealed trait Expr extends IRElem
object Expr extends Parser[Expr]

// pure value
case class ENum(n: Double) extends Expr with DoubleEquals(n)
case class EINum(n: Long) extends Expr
case class EBigINum(b: BigInt) extends Expr
case class EStr(str: String) extends Expr
case class EBool(b: Boolean) extends Expr
case object EUndef extends Expr
case object ENull extends Expr
case object EAbsent extends Expr
case class EConst(name: String) extends Expr
case class EClo(params: List[Id], captured: List[Id], body: Inst) extends Expr
case class ECont(params: List[Id], body: Inst) extends Expr

// completion
case class EComp(ty: Expr, value: Expr, target: Expr) extends Expr
case class EIsCompletion(expr: Expr) extends Expr
case class EReturnIfAbrupt(expr: Expr, check: Boolean) extends Expr

// IR objects
case class EMap(ty: Ty, props: List[(Expr, Expr)]) extends Expr
case class EList(exprs: List[Expr]) extends Expr
case class ESymbol(desc: Expr) extends Expr
case class ENotSupported(msg: String) extends Expr
case class EPop(list: Expr, idx: Expr) extends Expr
case class EContains(list: Expr, elem: Expr) extends Expr
case class ECopy(obj: Expr) extends Expr
case class EKeys(mobj: Expr, intSorted: Boolean) extends Expr
case class EIsInstanceOf(base: Expr, name: String) extends Expr

// etc
case class ERef(ref: Ref) extends Expr
case class EUOp(uop: UOp, expr: Expr) extends Expr
case class EBOp(bop: BOp, left: Expr, right: Expr) extends Expr
case class EConvert(source: Expr, target: COp, radixOpt: Option[Expr])
  extends Expr
case class ETypeOf(expr: Expr) extends Expr

/** TODO AST-related expressions */
sealed trait ASTExpr extends Expr
case class EGetElems(base: Expr, name: String) extends ASTExpr
case class EGetSyntax(base: Expr) extends ASTExpr
case class EParseSyntax(code: Expr, rule: Expr, parserParams: List[Boolean])
  extends ASTExpr

// -----------------------------------------------------------------------------
// IR References
// -----------------------------------------------------------------------------
sealed trait Ref extends IRElem
object Ref extends Parser[Ref]

/** reference identifiers * */
case class RefId(id: Id) extends Ref

/** reference properties * */
case class RefProp(ref: Ref, expr: Expr) extends Ref

// -----------------------------------------------------------------------------
// IR Identifiers
// -----------------------------------------------------------------------------
case class Id(name: String) extends IRElem
object Id extends Parser[Id]

// -----------------------------------------------------------------------------
// IR Types
// -----------------------------------------------------------------------------
case class Ty(name: String) extends IRElem
object Ty extends Parser[Ty]

// -----------------------------------------------------------------------------
// IR Operators
// -----------------------------------------------------------------------------
enum UOp extends IRElem:
  case Neg, Not, BNot
object UOp extends Parser[UOp]

/** IR Binary Operators */
enum BOp extends IRElem:
  case Plus, Sub, Mul, Pow, Div, UMod, Mod, Lt, Eq, Equal, And, Or, Xor, BAnd,
  BOr, BXOr, LShift, SRShift, URShift
object BOp extends Parser[BOp]

/** IR Convert Operators */
enum COp extends IRElem:
  case StrToNum, StrToBigInt, NumToStr, NumToInt, NumToBigInt, BigIntToNum
object COp extends Parser[COp]
