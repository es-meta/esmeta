package esmeta.cfg

import esmeta.util.{DoubleEquals, UId, Loc}

// -----------------------------------------------------------------------------
// Control-Flow Graphs (CFGs)
// -----------------------------------------------------------------------------
case class CFG(
  main: Int,
  funcs: List[Func],
) extends CFGElem {
  lazy val funcMap: Map[Int, Func] =
    (for (func <- funcs) yield func.id -> func).toMap
  lazy val nodeMap: Map[Int, Node] = (for {
    func <- funcs
    node <- func.nodes
  } yield node.id -> node).toMap
  lazy val funcOf: Map[Node, Func] = (for {
    func <- funcs
    node <- func.nodes
  } yield node -> func).toMap
}
object CFG extends Parser.From[CFG]

// -----------------------------------------------------------------------------
// Functions
// -----------------------------------------------------------------------------
case class Func(
  id: Int,
  kind: Func.Kind,
  name: String,
  params: List[Param],
  entry: Entry,
  blocks: List[Block],
  exit: Exit,
) extends CFGElem
  with UId {
  lazy val nodes: List[Node] =
    entry :: exit :: blocks
  lazy val nodeMap: Map[Int, Node] =
    (for (node <- nodes) yield node.id -> node).toMap
}
object Func extends Parser.From[Func]:
  enum Kind extends CFGElem:
    case AbsOp, NumMeth, SynDirOp, ConcMeth, Builtin, Clo, Cont
  object Kind extends Parser.From[Kind]

// -----------------------------------------------------------------------------
// Function Parameters
// -----------------------------------------------------------------------------
case class Param(lhs: Name, kind: Param.Kind, ty: Type) extends CFGElem
object Param extends Parser.From[Param]:
  enum Kind extends CFGElem:
    case Normal, Optional
  object Kind extends Parser.From[Kind]

// -----------------------------------------------------------------------------
// Nodes
// -----------------------------------------------------------------------------
sealed trait Node extends CFGElem with UId
object Node extends Parser.From[Node]

// entry nodes
case class Entry(id: Int, var next: Int) extends Node

// exit nodes
case class Exit(id: Int) extends Node

// block nodes
sealed trait Block extends Node

// lienar nodes
case class Linear(
  id: Int,
  var insts: Vector[Inst],
  var next: Int,
) extends Block

// branch nodes
case class Branch(
  id: Int,
  kind: Branch.Kind,
  cond: Expr,
  loc: Option[Loc],
  var thenNode: Int,
  var elseNode: Int,
) extends Block
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
) extends Block

// -----------------------------------------------------------------------------
// Instructions
// -----------------------------------------------------------------------------
sealed trait Inst extends CFGElem { val loc: Option[Loc] }
object Inst extends Parser.From[Inst]
case class IExpr(expr: Expr, loc: Option[Loc]) extends Inst
case class ILet(lhs: Name, expr: Expr, loc: Option[Loc]) extends Inst
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
case class EClo(fid: Int, captured: List[Name]) extends Expr
case class ECont(fid: Int) extends Expr

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

sealed trait Local extends Id
case class Name(name: String) extends Local
case class Temp(idx: Int) extends Local

// -----------------------------------------------------------------------------
// TODO Types
// -----------------------------------------------------------------------------
case class Type(name: String) extends CFGElem
object Type extends Parser.From[Type]
