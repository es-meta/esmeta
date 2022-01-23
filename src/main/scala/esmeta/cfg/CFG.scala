package esmeta.cfg

import esmeta.util.{DoubleEquals, UId, Loc}

// -----------------------------------------------------------------------------
// Control-Flow Graphs (CFGs)
// -----------------------------------------------------------------------------
case class CFG(
  main: Int,
  funcs: Map[Int, Func],
  nodes: Map[Int, Node],
) extends CFGElem
object CFG extends Parser.From[CFG]

// -----------------------------------------------------------------------------
// Functions
// -----------------------------------------------------------------------------
case class Func(
  id: Int,
  kind: Func.Kind,
  name: String,
  params: List[Param],
  entry: Int,
  exit: Int,
  nodes: Map[Int, Node],
) extends CFGElem
  with UId[Func]
object Func extends Parser.From[Func]:
  enum Kind extends CFGElem:
    case AbsOp, NumMeth, SynDirOp, ConcMeth, BuiltinMeth, Clo, Cont
  object Kind extends Parser.From[Kind]

// -----------------------------------------------------------------------------
// Function Parameters
// -----------------------------------------------------------------------------
case class Param(lhs: Local, ty: Type) extends CFGElem
object Param extends Parser.From[Param]

// -----------------------------------------------------------------------------
// Nodes
// -----------------------------------------------------------------------------
sealed trait Node extends CFGElem with UId[Node]
object Node extends Parser.From[Node]

// entry nodes
case class Entry(id: Int, var next: Int) extends Node

// exit nodes
case class Exit(id: Int) extends Node

// block nodes
case class Block(
  id: Int,
  var insts: Vector[Inst],
  var next: Int,
) extends Node

// branch nodes
case class Branch(
  id: Int,
  kind: Branch.Kind,
  cond: Expr,
  loc: Option[Loc],
  var thenNode: Int,
  var elseNode: Int,
) extends Node
object Branch:
  enum Kind extends CFGElem:
    case If, While, Foreach
  object Kind extends Parser.From[Kind]

// call nodes
case class Call(
  id: Int,
  lhs: Id,
  fexpr: Expr,
  args: List[Expr],
  loc: Option[Loc],
  var next: Int,
) extends Node

// -----------------------------------------------------------------------------
// Instructions
// -----------------------------------------------------------------------------
sealed trait Inst extends CFGElem { val loc: Option[Loc] }
object Inst extends Parser.From[Inst]
case class ILet(lhs: Local, expr: Expr, loc: Option[Loc]) extends Inst
case class IAssign(ref: Ref, expr: Expr, loc: Option[Loc]) extends Inst
case class IDelete(ref: Ref, loc: Option[Loc]) extends Inst
case class IPush(from: Expr, to: Expr, front: Boolean, loc: Option[Loc])
  extends Inst
case class IReturn(expr: Expr, loc: Option[Loc]) extends Inst
case class IAssert(expr: Expr, loc: Option[Loc]) extends Inst
case class IPrint(expr: Expr, loc: Option[Loc]) extends Inst

// -----------------------------------------------------------------------------
// Expressions
// -----------------------------------------------------------------------------
sealed trait Expr extends CFGElem
object Expr extends Parser.From[Expr]
case class EComp(tyExpr: Expr, tgtExpr: Expr, valExpr: Expr) extends Expr
case class EIsCompletion(expr: Expr) extends Expr
case class EReturnIfAbrupt(expr: Expr, check: Boolean) extends Expr
case class EPop(list: Expr, front: Boolean) extends Expr
case class EYet(msg: String) extends Expr
case class EContains(list: Expr, elem: Expr) extends Expr
case class ERef(ref: Ref) extends Expr
case class EUnary(uop: UOp, expr: Expr) extends Expr
case class EBinary(bop: BOp, left: Expr, right: Expr) extends Expr
case class EConvert(cop: COp, expr: Expr) extends Expr
case class ETypeOf(base: Expr) extends Expr
case class ETypeCheck(base: Expr, ty: Type) extends Expr

// allocation expressions
sealed trait AllocExpr extends Expr { val asite: Int }
case class EMap(tname: String, props: List[(Expr, Expr)], asite: Int)
  extends AllocExpr
case class EList(exprs: List[Expr], asite: Int) extends AllocExpr
case class ESymbol(desc: Expr, asite: Int) extends AllocExpr
case class ECopy(obj: Expr, asite: Int) extends AllocExpr
case class EKeys(map: Expr, intSorted: Boolean, asite: Int) extends AllocExpr

// literals
sealed trait Literal extends Expr
case class EMathVal(n: BigDecimal) extends Literal
case class ENumber(n: Double) extends Literal with DoubleEquals(n)
case class EBigInt(n: BigInt) extends Literal
case class EStr(str: String) extends Literal
case class EBool(b: Boolean) extends Literal
case object EUndef extends Literal
case object ENull extends Literal
case object EAbsent extends Literal
case class EConst(name: String) extends Literal
case class EClo(fid: Int, captured: List[Local]) extends Literal

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
  case ToStr(radix: Option[Expr])
object COp extends Parser.From[COp]

// -----------------------------------------------------------------------------
// References
// -----------------------------------------------------------------------------
sealed trait Ref extends CFGElem
object Ref extends Parser.From[Ref]

case class Prop(ref: Ref, expr: Expr) extends Ref

sealed trait Id extends Ref
case class Global(name: String) extends Id
case class Local(name: String) extends Id
case class Temp(id: Int) extends Id

// -----------------------------------------------------------------------------
// TODO Types
// -----------------------------------------------------------------------------
case class Type(name: String) extends CFGElem
object Type extends Parser.From[Type]
