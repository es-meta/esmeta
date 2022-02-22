package esmeta.interp

import scala.collection.mutable.{Map => MMap}
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.*
import esmeta.util.BaseUtils.*
import esmeta.util.DoubleEquals

// -----------------------------------------------------------------------------
// States
// -----------------------------------------------------------------------------
case class State(
  val cfg: CFG,
  var context: Context,
  var callStack: List[CallContext] = Nil,
  val globals: MMap[Global, Value] = MMap(),
  val heap: Heap = Heap(),
) extends InterpElem
object State { def apply(cfg: CFG): State = State(cfg, Context(cfg.main)) }

// -----------------------------------------------------------------------------
// Contexts
// -----------------------------------------------------------------------------
case class Context(
  val func: Func,
  val locals: MMap[Local, Value] = MMap(),
) extends InterpElem {
  var cursor: Cursor = func.entry.fold(ExitCursor(func))(NodeCursor(_))
  var retVal: Option[Value] = None
}

// -----------------------------------------------------------------------------
// Cursor
// -----------------------------------------------------------------------------
sealed trait Cursor extends InterpElem
case class NodeCursor(node: Node) extends Cursor
case class ExitCursor(func: Func) extends Cursor

// -----------------------------------------------------------------------------
// Calling Contexts
// -----------------------------------------------------------------------------
case class CallContext(retId: Id, context: Context) extends InterpElem

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
  var ty: String, // TODO handle type
  val props: MMap[PureValue, MapObj.Prop],
  var size: Int,
) extends Obj
case class ListObj(var values: Vector[PureValue] = Vector()) extends Obj
case class SymbolObj(desc: PureValue) extends Obj
case class YetObj(tname: String, msg: String) extends Obj

object MapObj {

  /** property values */
  case class Prop(value: Value, creationTime: Int)

  /** apply with type model */
  def apply(tname: String)(props: (PureValue, Value)*)(using
    cfg: CFG,
    typeModel: Option[TypeModel],
  ): MapObj =
    val obj: MapObj = MapObj(tname)
    for { ((k, v), idx) <- props.zipWithIndex }
      obj.props += k -> Prop(v, idx + obj.size)
    obj.size += props.size
    obj

  def apply(tname: String)(using
    cfg: CFG,
    typeModel: Option[TypeModel],
  ): MapObj =
    // TODO do not explicitly store methods in object but use a type model when
    // accessing methods
    val methods = typeModel.fold(Map())(_.apply(tname))
    val obj = MapObj(tname, MMap(), methods.size)
    for { ((name, fname), idx) <- methods.zipWithIndex }
      obj.props += Str(name) -> Prop(Clo(cfg.fnameMap(fname), Map()), idx)
    obj
}

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
case class Clo(func: Func, captured: Map[Name, Value]) extends PureValue

/** continuations */
case class Cont(
  func: Func,
  captured: Map[Name, Value],
  callStack: List[CallContext],
) extends PureValue

/** abstract syntax tree (AST) values */
case class AstValue(ast: Ast) extends PureValue

/** grammars */
case class Grammar(name: String, params: List[Boolean]) extends PureValue

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
