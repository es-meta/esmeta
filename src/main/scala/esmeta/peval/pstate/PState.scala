package esmeta.peval.pstate

import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.ir.{Expr, Field, Func, Global, Local, Name, Ref, Temp, Var}
import esmeta.peval.domain.*
import esmeta.state.*
import scala.collection.mutable.{Map => MMap}
import scala.util.{Try}

/** IR State for partial Evaluation. similar to state/State.scala */
case class PState private (
  var context: PContext,
  var callStack: List[PCallContext] = Nil,
  val globals: MMap[Global, PValue] = MMap(),
  val heap: PHeap = PHeap(),
) extends StateElem {

  override def clone: PState =
    new PState(context.copied, List.from(callStack), globals, heap)

  /** safe getter */
  def get(rt: RefTarget): Try[PValue] = Try(apply(rt))
  def get(x: Var): Try[PValue] = Try(apply(x))
  def get(base: Value, field: Value): Try[PValue] = Try(apply(base, field))

  /** getter */
  def apply(rt: RefTarget): PValue = rt match
    case VarTarget(x)             => apply(x)
    case FieldTarget(base, field) => apply(base, field)

  /** variable getter */
  def apply(x: Var): PValue = x match
    case x: Global => globals.getOrElse(x, throw UnknownVar(x))
    case x: Local  => context.locals.getOrElse(x, throw UnknownVar(x))

  /** field getter */
  def apply(base: Value, field: Value): PValue = base match
    case addr: Addr    => heap(addr, field)
    case AstValue(ast) => ??? // AstValue(ast(field))
    case Str(str)      => apply(str, field)
    case v             => throw InvalidRefBase(v)

  /* abstract getter */
  def apply(prt: PRefTarget): PValue = prt.tgt match
    case ARefTarget.AVarTarget(x)             => apply(x)
    case ARefTarget.AFieldTarget(base, field) => apply(base, field)

  /** abstract field getter */
  def apply(base: PValue, field: PValue): PValue = ???

  /** string field getter */
  def apply(str: String, field: Value): PValue = field match
    case Str("length") => Math(BigDecimal(str.length)).toPValue
    case Math(k)       => CodeUnit(str(k.toInt)).toPValue
    case _             => throw WrongStringRef(str, field)

  /** address getter */
  def apply(addr: Addr): PObj = heap(addr)

  /** define variables */
  def define(x: Var, pv: PValue): Unit = x match
    case x: Global => globals += x -> pv
    case x: Local  => context.locals += x -> pv

  /** setter */
  def update(rt: RefTarget, pv: PValue): Unit = rt match
    case VarTarget(x)             => update(x, pv)
    case FieldTarget(base, field) => update(base, field, pv)

  /** partial setter */
  def update(prt: PRefTarget, pv: PValue): Unit = prt.tgt match
    case ARefTarget.AVarTarget(x)             => update(x, pv)
    case ARefTarget.AFieldTarget(base, field) => update(base, field, pv)

  /** variable setter */
  def update(x: Var, pv: PValue): Unit = x match
    case x: Global => globals += x -> pv
    case x: Local  => context.locals += x -> pv

  /** field setter */
  def update(base: Value, field: Value, expr: PValue): Unit = ???

  /** abstract field setter */
  def update(base: PValue, field: PValue, expr: PValue): Unit = ???

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

  /** expand a field of a record object (abstract) */
  def expand(base: PValue, field: PValue): Unit =
    ??? // heap.expand(base.asAddr, field)

  /** delete a key from an map object */
  def delete(base: PValue, key: PValue): Unit =
    ??? //  heap.delete(base.asAddr, key)

  /** push a value to a list */
  def push(addr: Addr, value: PValue, front: Boolean): Unit =
    heap.push(addr, value, front)

  /** pop a value from a list */
  def pop(addr: Addr, front: Boolean): PValue = heap.pop(addr, front)

  /** copy object */
  def copy(addr: Addr): Addr = heap.copy(addr)

  /** get keys of a record/map object as a list */
  def keys(addr: Addr, intSorted: Boolean): Addr = heap.keys(addr, intSorted)

  /** allocate a record object */
  def allocRecord(
    tname: String,
    pairs: Iterable[(String, PValue)] = Nil,
  )(using CFG): Addr = heap.allocRecord(tname, pairs)

  /** allocate a map object */
  def allocMap(pairs: Iterable[(Value, PValue)]): Addr = heap.allocMap(pairs)

  /** allocate a map object with abstract pairs */
  def allocMapAbs(pairs: Iterable[(PValue, PValue)]): Addr = ???

  /** allocate a list object */
  def allocList(vs: Iterable[PValue]): Addr = heap.allocList(vs)

}

object PState {
  def apply(func: Func): PState = new PState(
    PContext(func),
    Nil,
    MMap(),
    PHeap(),
  )
}
