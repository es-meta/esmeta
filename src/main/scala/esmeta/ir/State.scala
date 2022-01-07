package esmeta.ir

import scala.collection.mutable.{Map => MMap}
import esmeta.util._
import esmeta.util.Useful._
// import esmeta.error.NotSupported

/** IR States */
case class State(
  var cursorGen: CursorGen[_ <: Cursor] = InstCursor,
  var context: Context = Context(),
  var ctxtStack: List[Context] = Nil,
  val globals: MMap[Id, Value] = MMap(),
  val heap: Heap = Heap(),
  var fnameOpt: Option[String] = None,
)

/** IR Contexts */
case class Context(
  var cursorOpt: Option[Cursor] = None,
  val retId: Id = ID_RETURN,
  val name: String = STR_TOP_LEVEL,
  // var prevCursorOpt: Option[Cursor] = None,
  // val astOpt: Option[AST] = None,
  // val algo: Option[Algo] = None,
  val locals: MMap[Id, Value] = MMap(),
  // val viewOpt: Option[View] = None
)

/** Evaluation cursors */
sealed trait Cursor {
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
sealed trait CursorGen[T <: Cursor] {
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
  map: MMap[Addr, Obj] = MMap(),
  var size: Int = 0,
)

/** IR Objects */
enum Obj(ty: Ty):
  case IRSymbol(desc: Value) extends Obj(Ty("Symbol"))
  // XXX
  // case IRMap(var ty: Ty, props: MMap[Value, (Value, Long)], var size: Long)
  //   extends Obj(ty)
  case IRMap(var ty: Ty, props: MMap[Value, Value], var size: Long)
    extends Obj(ty)
  case IRList(var values: Vector[Value] = Vector()) extends Obj(Ty("List"))
  case IRNotSupported(tyname: String, desc: String) extends Obj(Ty(tyname))

/** IR Reference Value */
enum RefValue:
  case RefValueId(id: Id)
  case RefValueProp(base: Value, prop: Value)

/** IR Values */
type Addr = Value.NamedAddr | Value.DynamicAddr
enum Value:
  case Num(double: Double)
  case INum(long: Long)
  case BigINum(b: BigInt)
  case Str(str: String)
  case Bool(bool: Boolean)
  case Undef
  case Null
  case Absent
  // case Func(algo: Algo)
  case Const(name: String)
  case NamedAddr(name: String)
  case DynamicAddr(long: Long)

  // completions
  case CompValue(
    ty: Const,
    value: Value,
    targetOpt: Option[String],
  )

  // closures
  case Clo(
    ctxtName: String,
    params: List[Id],
    locals: MMap[Id, Value],
    cursorOpt: Option[Cursor],
  )

  // continuations
  case Cont(
    params: List[Id],
    context: Context,
    ctxtStack: List[Context],
  )

// AST values
// case class ASTVal(ast: AST) extends Value
