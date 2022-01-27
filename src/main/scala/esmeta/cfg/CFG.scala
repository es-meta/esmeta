package esmeta.cfg

import esmeta.cfg.Utils.*
import esmeta.util.{DoubleEquals, UId, Locational}
import scala.collection.mutable.ListBuffer

// -----------------------------------------------------------------------------
// Control-Flow Graphs (CFGs)
// -----------------------------------------------------------------------------
case class CFG(
  main: Func,
  funcs: ListBuffer[Func],
) extends CFGElem {
  lazy val funcMap: Map[Int, Func] =
    (for (func <- funcs) yield func.id -> func).toMap
  lazy val fnameMap: Map[String, Func] =
    (for (func <- funcs) yield func.name -> func).toMap
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
  main: Boolean,
  kind: Func.Kind,
  name: String,
  params: List[Param],
  entry: Option[Node],
) extends CFGElem
  with UId {
  lazy val nodes: Set[Node] = entry.fold(Set())(reachable)
  lazy val nodeMap: Map[Int, Node] =
    (for (node <- nodes) yield node.id -> node).toMap
}
object Func extends Parser.From[Func]:
  enum Kind extends CFGElem:
    case AbsOp, NumMeth, SynDirOp, ConcMeth, InternalMeth, Builtin, Clo, Cont
  object Kind extends Parser.From[Kind]

/** function parameters */
case class Param(lhs: Name, kind: Param.Kind, ty: Type) extends CFGElem
object Param extends Parser.From[Param]:
  enum Kind extends CFGElem:
    case Normal, Optional
  object Kind extends Parser.From[Kind]

// -----------------------------------------------------------------------------
// Nodes
// -----------------------------------------------------------------------------
sealed trait Node extends CFGElem with UId with Locational
object Node extends Parser.From[Node]

/** block nodes */
case class Block(
  id: Int,
  var insts: ListBuffer[Inst] = ListBuffer(),
  var next: Option[Node] = None,
) extends Node

/** call nodes */
case class Call(
  id: Int,
  lhs: Id,
  fexpr: Expr,
  args: List[Expr],
  var next: Option[Node] = None,
) extends Node

/** branch nodes */
case class Branch(
  id: Int,
  kind: Branch.Kind,
  cond: Expr,
  var thenNode: Option[Node] = None,
  var elseNode: Option[Node] = None,
) extends Node
object Branch:
  enum Kind extends CFGElem:
    case If
    case Loop(str: String)
  object Kind extends Parser.From[Kind]

// -----------------------------------------------------------------------------
// Instructions
// -----------------------------------------------------------------------------
sealed trait Inst extends CFGElem with Locational
object Inst extends Parser.From[Inst]
case class IExpr(expr: Expr) extends Inst
case class ILet(lhs: Name, expr: Expr) extends Inst
case class IAssign(ref: Ref, expr: Expr) extends Inst
case class IDelete(ref: Ref) extends Inst
case class IPush(from: Expr, to: Expr, front: Boolean) extends Inst
case class IReturn(expr: Expr) extends Inst
case class IAssert(expr: Expr) extends Inst
case class IPrint(expr: Expr) extends Inst

// -----------------------------------------------------------------------------
// Expressions
// -----------------------------------------------------------------------------
sealed trait Expr extends CFGElem with Locational
object Expr extends Parser.From[Expr]
case class EComp(tyExpr: Expr, valExpr: Expr, tgtExpr: Expr) extends Expr
case class EIsCompletion(expr: Expr) extends Expr
case class EReturnIfAbrupt(expr: Expr, check: Boolean) extends Expr
case class EPop(list: Expr, front: Boolean) extends Expr
case class EYet(msg: String) extends Expr
case class EContains(list: Expr, elem: Expr) extends Expr
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
  // mathematic values
  case Abs, Floor
  // numeric values
  case Neg
  // boolean
  case Not
  // bitwise
  case BNot
object UOp extends Parser.From[UOp]

// binary operators
enum BOp extends CFGElem:
  // equality (e.g. is, are)
  case Eq
  // numeric values
  case Plus, Sub, Mul, Pow, Div, UMod, Mod, Lt, Equal
  // bitwise
  case BAnd, BOr, BXOr
  // shift
  case LShift, SRShift, URShift
  // boolean
  case And, Or, Xor
  // string
  case Concat, StrLt
object BOp extends Parser.From[BOp]

// variadic operators
enum VOp extends CFGElem:
  // mathematic values
  case Min, Max
object VOp extends Parser.From[VOp]

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
