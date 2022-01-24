package esmeta.interp

import scala.collection.mutable.{Map => MMap}
import esmeta.cfg.*
import esmeta.util.BaseUtils.*
import esmeta.util.DoubleEquals

// -----------------------------------------------------------------------------
// States
// -----------------------------------------------------------------------------
case class State(val cfg: CFG) extends InterpElem {
  var context: Context = Context(cfg.funcMap(cfg.main))
  var ctxtStack: List[Context] = Nil
  val globals: MMap[Global, Value] = MMap()
  val heap: Heap = Heap()
}

// -----------------------------------------------------------------------------
// Contexts
// -----------------------------------------------------------------------------
case class Context(
  val func: Func,
  val retId: Id = GLOBAL_RESULT,
  val ast: Option[AST] = None,
) extends InterpElem {
  var cur: Node = func.entry
  def name: String = func.name
  var retVal: Option[Value] = None
  val locals: MMap[Local, Value] = MMap()
}

// -----------------------------------------------------------------------------
// Heaps
// -----------------------------------------------------------------------------
case class Heap(
  val map: MMap[Addr, Obj] = MMap(),
  var size: Int = 0,
) extends InterpElem

// -----------------------------------------------------------------------------
// Objects
// -----------------------------------------------------------------------------
sealed trait Obj extends InterpElem
case class MapObj(
  var tname: String,
  val props: MMap[PureValue, MapProp] = MMap(),
  var size: Int = 0,
) extends Obj
case class ListObj(var values: Vector[PureValue] = Vector()) extends Obj
case class SymbolObj(desc: PureValue) extends Obj
case class YetObj(tname: String, msg: String) extends Obj

/** property values */
case class MapProp(value: Value, creationTime: Int)

// -----------------------------------------------------------------------------
// Reference Value
// -----------------------------------------------------------------------------
sealed trait RefValue extends InterpElem
case class IdValue(id: Id) extends RefValue
case class PropValue(base: Value, prop: PureValue) extends RefValue

// -----------------------------------------------------------------------------
// Values
// -----------------------------------------------------------------------------
sealed trait Value extends InterpElem

/** completion values */
case class Comp(
  ty: Const,
  value: PureValue,
  target: Option[String],
) extends Value

/** pure values (not completion values) */
sealed trait PureValue extends Value

/** addresses */
sealed trait Addr extends PureValue
case class NamedAddr(name: String) extends Addr
case class DynamicAddr(long: Long) extends Addr

/** closures */
case class Clo(fid: Int, captured: Map[Name, Value]) extends PureValue

/** continuations */
case class Cont(
  fid: Int,
  captured: Map[Name, Value],
  ctxtStack: List[Context],
) extends PureValue

/** AST values */
case class AST(
  name: String,
  args: List[Boolean],
  rhsIdx: Int,
  bits: Int,
  children: List[AST],
) extends PureValue

// -----------------------------------------------------------------------------
// Literal Values
// -----------------------------------------------------------------------------
sealed trait LiteralValue extends PureValue
sealed trait Numeric extends LiteralValue
case class Math(n: BigDecimal) extends Numeric
case class Number(n: Double) extends Numeric with DoubleEquals(n)
case class BigInt(n: scala.math.BigInt) extends Numeric
case class Str(str: String) extends LiteralValue
case class Bool(bool: Boolean) extends LiteralValue
case object Undef extends LiteralValue
case object Null extends LiteralValue
case object Absent extends LiteralValue
case class Const(name: String) extends LiteralValue
