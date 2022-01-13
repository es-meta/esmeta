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
  var result: Option[Value] = None,
) extends IRElem

// -----------------------------------------------------------------------------
// IR Contexts
// -----------------------------------------------------------------------------
case class Context(
  var cursorOpt: Option[Cursor] = None,
  val retId: Id = ID_RETURN,
  val name: String = STR_TOP_LEVEL,
  val locals: MMap[Id, Value] = MMap(),
  // TODO val astOpt: Option[AST] = None,
  // TODO val algo: Option[Algo] = None,
) extends IRElem

// -----------------------------------------------------------------------------
// Evaluation cursors
// -----------------------------------------------------------------------------
sealed trait Cursor extends IRElem
case class InstCursor(curr: Inst, rest: List[Inst]) extends Cursor

/** IR Cursor Generator */
sealed trait CursorGen[T <: Cursor]:
  def apply(inst: Inst): Option[T]

// instruction cursors
object InstCursor extends CursorGen[InstCursor]:
  def apply(inst: Inst): Option[InstCursor] = Some(InstCursor(inst, Nil))
  def from(insts: List[Inst]): Option[InstCursor] = insts match
    case cur :: rest => Some(InstCursor(cur, rest))
    case Nil         => None

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
sealed trait Obj(var ty: Ty) extends IRElem
case class IRSymbol(desc: PureValue) extends Obj(TY_SYMBOL)
case class IRMap(t: Ty, props: MMap[PureValue, IRMapValue], var size: Long)
  extends Obj(t)
case class IRList(var values: Vector[PureValue] = Vector()) extends Obj(TY_LIST)
case class IRNotSupported(tyname: String, desc: String) extends Obj(Ty(tyname))

/** values for IRMap structures */
case class IRMapValue(value: Value, creationTime: Long)

// -----------------------------------------------------------------------------
// IR Reference Value
// -----------------------------------------------------------------------------
sealed trait RefValue extends IRElem
case class RefValueId(id: Id) extends RefValue
case class RefValueProp(base: Value, prop: PureValue) extends RefValue

// -----------------------------------------------------------------------------
// IR Values
// -----------------------------------------------------------------------------
sealed trait Value extends IRElem

/** completion values */
case class CompValue(
  ty: Const,
  value: PureValue,
  target: Str | Const = CONST_EMPTY,
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
