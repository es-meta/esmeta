package esmeta.peval

import esmeta.ir.{Expr, Field, Func, Global, Local, Name, Ref, Temp, Var}
import esmeta.state.{
  Addr,
  FieldTarget,
  Heap,
  Obj,
  RefTarget,
  StateElem,
  Value,
  VarTarget,
}
import scala.collection.mutable.{Map => MMap}
import scala.util.{Try}

/** IR State for partial Evaluation. similar to state/State.scala */
case class PState private (
  var context: PContext,
  var callStack: List[PCallContext] = Nil,
  val globals: MMap[Global, Expr] = MMap(),
  val heap: Heap = Heap(),
) extends StateElem {

  /** safe getter */
  def get(rt: RefTarget): Try[Value] = ??? // Try(apply(rt))
  def get(x: Var): Try[Value] = ??? // Try(apply(x))
  def get(base: Value, field: Value): Try[Value] =
    ??? // Try(apply(base, field))

  /** getter */
  def apply(rt: RefTarget): Value = ???
  //  rt match
  //   case VarTarget(x)             => apply(x)
  //   case FieldTarget(base, field) => apply(base, field)

  /** variable getter */
  def apply(x: Var): Value = ???
  // x match
  // case x: Global => globals.getOrElse(x, throw UnknownVar(x))
  // case x: Local  => context.locals.getOrElse(x, throw UnknownVar(x))

  /** field getter */
  def apply(base: Value, field: Value): Value = ???
  //  base match
  //   case addr: Addr    => heap(addr, field)
  //   case AstValue(ast) => AstValue(ast(field))
  //   case Str(str)      => apply(str, field)
  //   case v             => throw InvalidRefBase(v)

  /** string field getter */
  def apply(str: String, field: Value): Value = ???
  // field match
  //   case Str("length") => Math(BigDecimal(str.length))
  //   case Math(k)       => CodeUnit(str(k.toInt))
  //   case _             => throw WrongStringRef(str, field)

  /** address getter */
  def apply(addr: Addr): Obj = heap(addr)

  /** define variables */
  def define(x: Var, expr: Expr): Unit = x match
    case x: Global => globals += x -> expr
    case x: Local  => context.locals += x -> expr

  /** setter */
  def update(rt: RefTarget, expr: Expr): Unit = rt match
    case VarTarget(x)             => update(x, expr)
    case FieldTarget(base, field) => update(base, field, expr)

  /** variable setter */
  def update(x: Var, expr: Expr): Unit = x match
    case x: Global => globals += x -> expr
    case x: Local  => context.locals += x -> expr

  /** field setter */
  def update(base: Value, field: Value, expr: Expr): Unit = ???

  /** existence checks */
  def exists(rt: RefTarget): Boolean = ???
  // rt match
  // case VarTarget(x)             => exists(x)
  // case FieldTarget(base, field) => exists(base, field)

  /** variable existence check */
  def exists(x: Var): Boolean = ???
  // x match
  // case x: Global => globals.contains(x)
  // case x: Local  => context.locals.contains(x)

  /** field existence check */
  def exists(base: Value, field: Value): Boolean = ???
  // base match
  //   case addr: Addr    => heap.exists(addr, field)
  //   case AstValue(ast) => ast.exists(field)
  //   case _             => error(s"illegal field existence check: $base[$field]")

  /** expand a field of a record object */
  def expand(base: Value, field: Value): Unit =
    ??? // heap.expand(base.asAddr, field)

  /** delete a key from an map object */
  def delete(base: Value, key: Value): Unit =
    ??? //  heap.delete(base.asAddr, key)

}

object PState {
  def apply(func: Func): PState = new PState(
    PContext(func),
    Nil,
    MMap(),
    Heap(),
  )
}
