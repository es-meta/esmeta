package esmeta.ir

import scala.collection.mutable.{Map => MMap}
import esmeta.util.*
import esmeta.util.BaseUtils.*

// -----------------------------------------------------------------------------
// IR States
// -----------------------------------------------------------------------------
case class State(
  var cursorGen: CursorGen[_ <: Cursor] = InstCursor,
  var context: Context = Context(),
  var ctxtStack: List[Context] = Nil,
  val globals: MMap[Id, Value] = MMap(),
  val heap: Heap = Heap(),
  var fnameOpt: Option[String] = None,
) extends IRElem

// -----------------------------------------------------------------------------
// IR Contexts
// -----------------------------------------------------------------------------
case class Context(
  var cursorOpt: Option[Cursor] = None,
  val retId: Id = ID_RETURN,
  val name: String = STR_TOP_LEVEL,
  // TODO var prevCursorOpt: Option[Cursor] = None,
  // TODO val astOpt: Option[AST] = None,
  // TODO val algo: Option[Algo] = None,
  val locals: MMap[Id, Value] = MMap(),
  // TODO val viewOpt: Option[View] = None
) extends IRElem

// -----------------------------------------------------------------------------
// Evaluation cursors
// -----------------------------------------------------------------------------
sealed trait Cursor extends IRElem {
  // next cursor
  def next: Option[Cursor] = this match {
    case InstCursor(_, rest) => InstCursor.from(rest)
  }
  // get instruction of current cursor
  def inst: Option[Inst] = this match {
    case InstCursor(cur, _) => Some(cur)
  }
}

// generator of evaluation cursors
sealed trait CursorGen[T <: Cursor] extends IRElem {
  def apply(inst: Inst): Option[T]
}

// instruction cursors
case class InstCursor(cur: Inst, rest: List[Inst]) extends Cursor
object InstCursor extends CursorGen[InstCursor] {
  def apply(
    inst: Inst,
  ): Option[InstCursor] = Some(InstCursor(inst, Nil))
  def from(insts: List[Inst]): Option[InstCursor] = insts match {
    case cur :: rest => Some(InstCursor(cur, rest))
    case Nil         => None
  }
}

// -----------------------------------------------------------------------------
// IR Heaps
// -----------------------------------------------------------------------------
case class Heap(
  map: MMap[Addr, Obj] = MMap(),
  var size: Int = 0,
) extends IRElem

// -----------------------------------------------------------------------------
// IR Objects
// -----------------------------------------------------------------------------
sealed trait Obj extends IRElem
case class IRSymbol(desc: Value) extends Obj
case class IRMap(var ty: Ty, props: MMap[Value, IRMapValue], var size: Long)
  extends Obj
case class IRList(var values: Vector[Value] = Vector()) extends Obj
case class IRNotSupported(tyname: String, desc: String) extends Obj

/** values for IRMap structures */
case class IRMapValue(value: Value, creationTime: Long)

// -----------------------------------------------------------------------------
// IR Reference Value
// -----------------------------------------------------------------------------
sealed trait RefValue extends IRElem
case class RefValueId(id: Id) extends RefValue
case class RefValueProp(base: Value, prop: Value) extends RefValue

// -----------------------------------------------------------------------------
// IR Values
// -----------------------------------------------------------------------------
sealed trait Value extends IRElem

/** completion values */
case class CompValue(
  ty: Const,
  value: Value,
  targetOpt: Option[String],
) extends Value

/** pure values (not completion values) */
sealed trait PureValue extends Value
// TODO case class Func(algo: Algo) extends PureValue

/** constants */
case class Const(name: String) extends PureValue

/** addresses */
sealed trait Addr extends PureValue
case class NamedAddr(name: String) extends Addr
case class DynamicAddr(long: Long) extends Addr

/** closures */
case class Clo(
  ctxtName: String,
  params: List[Id],
  locals: MMap[Id, Value],
  cursorOpt: Option[Cursor],
) extends PureValue

/** continuations */
case class Cont(
  params: List[Id],
  context: Context,
  ctxtStack: List[Context],
) extends PureValue

/** continuations */
sealed trait SimpleValue extends PureValue
case class Num(double: Double) extends SimpleValue
case class INum(long: Long) extends SimpleValue
case class BigINum(b: BigInt) extends SimpleValue
case class Str(str: String) extends SimpleValue
case class Bool(bool: Boolean) extends SimpleValue
case object Undef extends SimpleValue
case object Null extends SimpleValue
case object Absent extends SimpleValue

// TODO AST values
// case class ASTVal(ast: AST) extends PureValue
