package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.Category.*
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
  var typeErrors: Set[TypeError] = Set(),
) extends StateElem {

  given CFG = cfg

  /** current function */
  def func: Func = context.cursor.func

  /** local environment */
  def locals: MMap[Local, Value] = context.locals

  /** safe getter */
  def get(rt: RefTarget): Try[Value] = Try(apply(rt))
  def get(x: Var): Try[Value] = Try(apply(x))
  def get(base: Value, field: Value): Try[Value] = Try(apply(base, field))

  /** getter */
  def apply(rt: RefTarget): Value = rt match
    case VarTarget(x)             => apply(x)
    case FieldTarget(base, field) => apply(base, field)

  /** variable getter */
  def apply(x: Var): Value = x match
    case x: Global => globals.getOrElse(x, throw UnknownVar(x))
    case x: Local  => context.locals.getOrElse(x, throw UnknownVar(x))

  /** field getter */
  def apply(base: Value, field: Value): Value = base match
    case addr: Addr    => heap(addr, field)
    case AstValue(ast) => AstValue(ast(field))
    case Str(str)      => apply(str, field)
    case v             => throw InvalidRefBase(v)

  /** string field getter */
  def apply(str: String, field: Value): Value = field match
    case Math(k) => CodeUnit(str(k.toInt))
    case _       => throw WrongStringRef(str, field)

  /** address getter */
  def apply(addr: Addr): Obj = heap(addr)

  /** define variables */
  def define(x: Var, value: Value): Unit = x match
    case x: Global => globals += x -> value
    case x: Local  => context.locals += x -> value

  /** setter */
  def update(rt: RefTarget, value: Value): Unit = rt match
    case VarTarget(x)             => update(x, value)
    case FieldTarget(base, field) => update(base, field, value)

  /** variable setter */
  def update(x: Var, value: Value): Unit = x match
    case x: Global => globals += x -> value
    case x: Local  => context.locals += x -> value

  /** field setter */
  def update(base: Value, field: Value, value: Value): Unit =
    heap.update(base.asAddr, field, value)

  /** existence checks */
  def exists(rt: RefTarget): Boolean = rt match
    case VarTarget(x)             => exists(x)
    case FieldTarget(base, field) => exists(base, field)

  /** variable existence check */
  def exists(x: Var): Boolean = x match
    case x: Global => globals.contains(x)
    case x: Local  => context.locals.contains(x)

  /** field existence check */
  def exists(base: Value, field: Value): Boolean = base match
    case addr: Addr    => heap.exists(addr, field)
    case AstValue(ast) => ast.exists(field)
    case _             => raise(s"illegal field existence check: $base[$field]")

  /** expand a field of a record object */
  def expand(base: Value, field: Value): Unit = heap.expand(base.asAddr, field)

  /** delete a key from an map object */
  def delete(base: Value, key: Value): Unit = heap.delete(base.asAddr, key)

  /** push a value to a list */
  def push(addr: Addr, value: Value, front: Boolean): Unit =
    heap.push(addr, value, front)

  /** pop a value from a list */
  def pop(addr: Addr, front: Boolean): Value = heap.pop(addr, front)

  /** copy object */
  def copy(addr: Addr): Addr = heap.copy(addr)

  /** get keys of a record/map object as a list */
  def keys(addr: Addr, intSorted: Boolean): Addr = heap.keys(addr, intSorted)

  /** allocate a record object */
  def allocRecord(
    tname: String,
    pairs: Iterable[(String, Value)] = Nil,
  )(using CFG): Addr = heap.allocRecord(tname, pairs)

  /** allocate a map object */
  def allocMap(pairs: Iterable[(Value, Value)]): Addr = heap.allocMap(pairs)

  /** allocate a list object */
  def allocList(vs: Iterable[Value]): Addr = heap.allocList(vs)

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

  /** get type of value in current state */
  def typeOf(value: Value, detail: Boolean = true): ValueTy = value match
    case addr: Addr          => typeOf(heap(addr), detail)
    case Clo(func, _)        => CloT(func.name)
    case Cont(func, _, _)    => ContT(func.id)
    case AstValue(ast)       => AstT(ast.name, ast.idx)
    case symb: GrammarSymbol => GrammarSymbolT(symb)
    case Math(d)             => MathT(d)
    case Infinity(pos)       => InfinityT(pos)
    case Enum(name)          => EnumT(name)
    case _: CodeUnit         => CodeUnitT
    case _: Number           => NumberT
    case _: BigInt           => BigIntT
    case _: Str              => StrT
    case _: Bool             => BoolT
    case Undef               => UndefT
    case Null                => NullT

  /** get type of addresses in current state */
  def typeOf(obj: Obj, detail: Boolean): ValueTy = obj match
    case RecordObj(tname, map) if detail =>
      // recursively get detailed types for completion records
      val recDetail = tname == "CompletionRecord"
      val fm = FieldMap(
        map.map { (k, v) => k -> Binding(typeOf(v, detail = recDetail)) }.toMap,
      )
      RecordT(tname, fm)
    case RecordObj(tname, _) => RecordT(tname)
    case m: MapObj           => MapT
    case YetObj(tname, _)    => throw NotSupported(Feature)(tname)
    case ListObj(values) => ListT(values.map(typeOf(_)).foldLeft(BotT)(_ || _))

  /** get string for a current cursor */
  def getCursorString: String = getCursorString(false)
  def getCursorString(location: Boolean): String = context.cursor match
    case NodeCursor(func, node, _) =>
      val irFunc = func.irFunc
      s"[${irFunc.kind}${irFunc.name}] ${node.toString(location = location)}"
    case ExitCursor(func) =>
      val irFunc = func.irFunc
      s"[${irFunc.kind}${irFunc.name}] Exited"

  /** get string for a given address */
  def getString(value: Value): String = value match
    case addr: Addr => addr.toString + " -> " + heap(addr).toString
    case _          => value.toString
}
object State {

  /** initialize states with a CFG */
  def apply(cfg: CFG): State = State(cfg, Context(cfg.main))

}
