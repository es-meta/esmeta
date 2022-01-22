package esmeta.cfg

import esmeta.util.{DoubleEquals, UId, Loc}

// -----------------------------------------------------------------------------
// Control-Flow Graphs (CFGs)
// -----------------------------------------------------------------------------
case class CFG(funcs: Set[Func]) extends CFGElem
object CFG extends Parser.From[CFG]

// -----------------------------------------------------------------------------
// Functions
// -----------------------------------------------------------------------------
case class Func(
  id: Int,
  kind: Func.Kind,
  params: List[Param],
  entry: Entry,
  exit: Exit,
  nodes: Set[Node],
) extends CFGElem
  with UId[Func]
object Func extends Parser.From[Func]:
  enum Kind extends CFGElem:
    case AbsOp, NumMeth, SynDirOp, ConcMeth, BuiltinMeth, Clo, Cont

// -----------------------------------------------------------------------------
// Function Parameters
// -----------------------------------------------------------------------------
case class Param(name: String, ty: Type) extends CFGElem
object Param extends Parser.From[Param]

// -----------------------------------------------------------------------------
// Nodes
// -----------------------------------------------------------------------------
sealed trait Node extends CFGElem with UId[Node] { val loc: Loc }
object Node extends Parser.From[Node]

// entry nodes
case class Entry(id: Int, loc: Loc, var next: Node) extends Node

// exit nodes
case class Exit(id: Int, loc: Loc) extends Node

// block nodes
case class Block(
  id: Int,
  loc: Loc,
  var insts: Vector[Inst],
  var next: Node,
) extends Node

// branch nodes
case class Branch(
  id: Int,
  loc: Loc,
  kind: Branch.Kind,
  cond: Expr,
  var thenNode: Node,
  var elseNode: Node,
) extends Node
object Branch:
  enum Kind extends CFGElem:
    case If, While, Foreach

// call nodes
case class Call(
  id: Int,
  loc: Loc,
  lhs: Id,
  func: Expr,
  args: List[Expr],
) extends Node

// -----------------------------------------------------------------------------
// Instructions
// -----------------------------------------------------------------------------
sealed trait Inst extends CFGElem { val loc: Loc }
object Inst extends Parser.From[Inst]
case class ILet(loc: Loc, lhs: Local, expr: Expr) extends Inst
case class IAssign(loc: Loc, ref: Ref, expr: Expr) extends Inst
case class IDelete(loc: Loc, ref: Ref) extends Inst
case class IPush(loc: Loc, from: Expr, to: Expr, front: Boolean) extends Inst
case class IReturn(loc: Loc, expr: Expr) extends Inst
case class IAssert(loc: Loc, expr: Expr) extends Inst
case class IPrint(loc: Loc, expr: Expr) extends Inst

// -----------------------------------------------------------------------------
// Expressions
// -----------------------------------------------------------------------------
sealed trait Expr extends CFGElem
object Expr extends Parser.From[Expr]

// completion expressions
sealed trait CompExpr extends Expr
case class EComp(tyExpr: Expr, valExpr: Expr, tgtExpr: Expr) extends CompExpr
case class EIsCompletion(expr: Expr) extends CompExpr
case class EReturnIfAbrupt(expr: Expr, check: Boolean) extends CompExpr

// allocation expressions
sealed trait AllocExpr extends Expr { val asite: Int }
case class EMap(tname: String, props: List[(Expr, Expr)], asite: Int)
  extends AllocExpr
case class EList(exprs: List[Expr], asite: Int) extends AllocExpr
case class ESymbol(desc: Expr, asite: Int) extends AllocExpr
case class ECopy(obj: Expr, asite: Int) extends AllocExpr
case class EKeys(map: Expr, intSorted: Boolean, asite: Int) extends AllocExpr

// update expressions
sealed trait UpdateExpr extends Expr
case class EPop(list: Expr, front: Boolean) extends UpdateExpr

// simple expressions
sealed trait SimpleExpr extends Expr
case class EYet(msg: String) extends SimpleExpr
case class EContains(list: Expr, elem: Expr) extends SimpleExpr
case class ERef(ref: Ref) extends SimpleExpr
case class EUnary(uop: UOp, expr: Expr) extends SimpleExpr
case class EBinary(bop: BOp, left: Expr, right: Expr) extends SimpleExpr
case class EConvert(cop: COp, expr: Expr) extends SimpleExpr
case class ETypeOf(base: Expr) extends SimpleExpr
case class ETypeCheck(base: Expr, ty: Type) extends SimpleExpr

// literals
sealed trait Literal extends SimpleExpr
case class EMathVal(n: BigDecimal) extends Literal
case class ENumber(n: Double) extends Literal with DoubleEquals(n)
case class EBigInt(n: BigInt) extends Literal
case class EStr(str: String) extends Literal
case class EBool(b: Boolean) extends Literal
case object EUndef extends Literal
case object ENull extends Literal
case object EAbsent extends Literal
case class EConst(name: String) extends Literal
case class EClo(func: Func, captured: List[Local]) extends Literal

// -----------------------------------------------------------------------------
// Operators
// -----------------------------------------------------------------------------
// unary operators
enum UOp extends CFGElem:
  case Neg, Not, BNot
object UOp extends Parser.From[UOp]

// binary operators
enum BOp extends CFGElem:
  case Plus, Sub, Mul, Pow, Div, UMod, Mod, Lt, Eq, Equal, And, Or, Xor, BAnd,
  BOr, BXOr, LShift, SRShift, URShift
object BOp extends Parser.From[BOp]

// conversion operators
enum COp extends CFGElem:
  case ToBigInt, ToNumber, ToMath
  case ToStr(radixOpt: Option[Expr])
object COp extends Parser.From[COp]

// -----------------------------------------------------------------------------
// References
// -----------------------------------------------------------------------------
sealed trait Ref extends CFGElem
object Ref extends Parser.From[Ref]

case class PropRef(ref: Ref, expr: Expr) extends Ref

sealed trait Id extends Ref
case class Global(name: String) extends Id
case class Local(name: String) extends Id
case class Temp(id: Int) extends Id

// -----------------------------------------------------------------------------
// TODO Types
// -----------------------------------------------------------------------------
case class Type(name: String) extends CFGElem
object Type extends Parser.From[Type]
