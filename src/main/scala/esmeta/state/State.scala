package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}
import scala.util.Try

/** IR states */
case class State(
  val cfg: CFG,
  var context: Context,
  val sourceText: Option[String] = None,
  val cachedAst: Option[Ast] = None,
  val filename: Option[String] = None,
  var callStack: List[CallContext] = Nil,
  val globals: MMap[Global, Value] = MMap(),
  val heap: Heap = Heap(),
) extends StateElem {

  given CFG = cfg

  /** current function */
  def func: Func = context.cursor match
    case NodeCursor(node) => cfg.funcOf(node)
    case ExitCursor(func) => func

  /** local enviornment */
  def locals: MMap[Local, Value] = context.locals

  /** safe getter */
  def get(rt: RefTarget): Try[Value] = Try(apply(rt))
  def get(x: Var): Try[Value] = Try(apply(x))
  def get(base: Value, field: Value): Try[Value] = Try(apply(base, field))

  /** getter */
  def apply(rt: RefTarget): Value = rt match
    case VarTarget(x)             => apply(x)
    case FieldTarget(base, field) => apply(base, field)
  def apply(x: Var): Value = x match
    case x: Global => globals.getOrElse(x, throw UnknownVar(x))
    case x: Local  => context.locals.getOrElse(x, throw UnknownVar(x))
  def apply(base: Value, field: Value): Value = base match
    case comp: Comp =>
      field match
        case Str("Type")   => comp.ty
        case Str("Value")  => comp.value
        case Str("Target") => comp.targetValue
        case _             => throw InvalidCompField(comp, field)
    case addr: Addr    => heap(addr, field)
    case AstValue(ast) => AstValue(ast(field))
    case Str(str)      => apply(str, field)
    case v             => throw InvalidRefBase(v)
  def apply(str: String, field: Value): Value = field match
    case Str("length") => Math(BigDecimal(str.length))
    case Math(k)       => CodeUnit(str(k.toInt))
    case _             => throw WrongStringRef(str, field)
  def apply(addr: Addr): Obj = heap(addr)

  /** setters */
  def define(x: Var, value: Value): Unit = x match
    case x: Global => globals += x -> value
    case x: Local  => context.locals += x -> value
  def update(rt: RefTarget, value: Value): Unit = rt match
    case VarTarget(x)             => update(x, value)
    case FieldTarget(base, field) => update(base, field, value)
  def update(x: Var, value: Value): Unit = x match
    case x: Global => globals += x -> value
    case x: Local  => context.locals += x -> value
  def update(base: Value, field: Value, value: Value): Unit = base match
    // XXX see https://github.com/es-meta/esmeta/issues/65
    case comp: Comp if comp.isAbruptCompletion && field.asStr == "Value" =>
      comp.value = value.toPureValue
    case addr: Addr => heap.update(addr, field, value)
    case _          => error(s"illegal field update: $base[$field] = $value")

  /** existence checks */
  def exists(rt: RefTarget): Boolean = rt match
    case VarTarget(x)             => exists(x)
    case FieldTarget(base, field) => exists(base, field)
  def exists(x: Var): Boolean = x match
    case x: Global => globals.contains(x)
    case x: Local  => context.locals.contains(x)
  def exists(base: Value, field: Value): Boolean = base match
    case addr: Addr    => heap.exists(addr, field)
    case AstValue(ast) => ast.exists(field)
    case _             => error(s"illegal field existence check: $base[$field]")

  /** expand */
  def expand(base: Value, field: Value): Unit = heap.expand(base.asAddr, field)

  /** delete */
  def delete(base: Value, key: Value): Unit = heap.delete(base.asAddr, key)

  /** push */
  def push(addr: Addr, value: Value, front: Boolean): Unit =
    heap.push(addr, value, front)
  def pop(addr: Addr, front: Boolean): Value =
    heap.pop(addr, front)
  def copy(addr: Addr): Addr =
    heap.copy(addr)
  def keys(addr: Addr, intSorted: Boolean): Addr =
    heap.keys(addr, intSorted)
  def allocRecord(tname: String)(using CFG): Addr =
    heap.allocRecord(tname)
  def allocMap: Addr =
    heap.allocMap
  def allocList(vs: Iterable[Value]): Addr =
    heap.allocList(vs)

  /** get string for a current cursor */
  def getCursorString: String = getCursorString(false)
  def getCursorString(location: Boolean): String = context.cursor match
    case NodeCursor(node) =>
      val irFunc = cfg.funcOf(node).irFunc
      s"[${irFunc.kind}${irFunc.name}] ${node.toString(location = location)}"
    case ExitCursor(func) =>
      val irFunc = func.irFunc
      s"[${irFunc.kind}${irFunc.name}] Exited"

  /** get string for a given address */
  def getString(value: Value): String = value match {
    case comp: Comp =>
      comp.toString + (comp.value match {
        case addr: Addr => " -> " + heap(addr).toString
        case _          => ""
      })
    case addr: Addr => addr.toString + " -> " + heap(addr).toString
    case _          => value.toString
  }

  /** copied */
  def copied: State =
    val newGlobals = MMap.from(globals)
    val newHeap = heap.copied
    val newContext = context.copied
    val newCallStack = callStack.map(_.copied)
    State(
      cfg,
      newContext,
      sourceText,
      cachedAst,
      filename,
      newCallStack,
      newGlobals,
      newHeap,
    )
}
object State {

  /** initialize states with a CFG */
  def apply(cfg: CFG): State = State(cfg, Context(cfg.main))

}
