package esmeta.ir

import scala.collection.mutable.{Map => MMap}
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** IR States */
case class State(
  var cursorGen: CursorGen[_ <: Cursor] = InstCursor,
  var context: Context = Context(),
  var ctxtStack: List[Context] = Nil,
  val globals: MMap[Id, Value] = MMap(),
  val heap: Heap = Heap(),
  var fnameOpt: Option[String] = None,
) extends IRElem

/** IR Contexts */
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

/** Evaluation cursors */
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

/** IR Heaps */
case class Heap(
  map: MMap[Value.Addr, Obj] = MMap(),
  var size: Int = 0,
) extends IRElem

/** IR Objects */
enum Obj extends IRElem:
  case IRSymbol(desc: Value)
  case IRMap(var ty: Ty, props: MMap[Value, IRMapValue], var size: Long)
  case IRList(var values: Vector[Value] = Vector())
  case IRNotSupported(tyname: String, desc: String)

/** values for IRMap structures */
case class IRMapValue(value: Value, creationTime: Long)

/** IR Reference Value */
enum RefValue extends IRElem:
  case RefValueId(id: Id)
  case RefValueProp(base: Value, prop: Value)

/** IR Values */
enum Value extends IRElem:
  case Num(double: Double)
  case INum(long: Long)
  case BigINum(b: BigInt)
  case Str(str: String)
  case Bool(bool: Boolean)
  case Undef
  case Null
  case Absent
  // TODO case Func(algo: Algo)
  case Const(name: String)
  case NamedAddr(name: String)
  case DynamicAddr(long: Long)
  case CompValue(
    ty: Const,
    value: Value,
    targetOpt: Option[String],
  )
  case Clo(
    ctxtName: String,
    params: List[Id],
    locals: MMap[Id, Value],
    cursorOpt: Option[Cursor],
  )
  case Cont(
    params: List[Id],
    context: Context,
    ctxtStack: List[Context],
  )
object Value {
  // addresses
  type Addr = NamedAddr | DynamicAddr
}

// TODO AST values
// case class ASTVal(ast: AST) extends Value
