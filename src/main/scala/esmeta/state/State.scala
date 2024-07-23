package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}

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

  /** get the current function */
  def func: Func = context.cursor match
    case NodeCursor(node) => cfg.funcOf(node)
    case ExitCursor(func) => func

  /** get local variable maps */
  def locals: MMap[Local, Value] = context.locals

  /** lookup variable directly */
  def directLookup(x: Var): Value = (x match {
    case x: Global => globals.get(x)
    case x: Local  => context.locals.get(x)
  }).getOrElse(throw UnknownId(x))

  /** getters */
  def apply(rt: RefTarget): Value = rt match
    case VarTarget(x)             => apply(x)
    case FieldTarget(base, field) => apply(base, field)
  def apply(x: Var): Value = directLookup(x) match
    case Absent if func.isBuiltin => Undef
    case v                        => v
  def apply(base: Value, field: PureValue): Value = base match
    case comp: Comp =>
      field match
        case Str("Type")   => comp.ty
        case Str("Value")  => comp.value
        case Str("Target") => comp.targetValue
        case _             => throw InvalidCompField(comp, field)
    case addr: Addr    => heap(addr, field)
    case AstValue(ast) => apply(ast, field)
    case Str(str)      => apply(str, field)
    case v             => throw InvalidRefBase(v)
  def apply(ast: Ast, field: PureValue): PureValue =
    (ast, field) match
      case (_, Str("parent")) => ast.parent.map(AstValue(_)).getOrElse(Absent)
      case (syn: Syntactic, Str(fieldStr)) =>
        val Syntactic(name, _, rhsIdx, children) = syn
        val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
        rhs.getNtIndex(fieldStr).flatMap(children(_)) match
          case Some(child) => AstValue(child)
          case _           => throw InvalidAstField(syn, Str(fieldStr))
      case (syn: Syntactic, Math(n)) if n.isValidInt =>
        syn.children(n.toInt).map(AstValue(_)).getOrElse(Absent)
      case _ => throw InvalidAstField(ast, field)
  def apply(str: String, field: PureValue): PureValue = field match
    case Str("length") => Math(BigDecimal(str.length))
    case Math(k)       => CodeUnit(str(k.toInt))
    case Number(k)     => CodeUnit(str(k.toInt))
    case _             => throw WrongStringRef(str, field)
  def apply(addr: Addr): Obj = heap(addr)

  /** setters */
  def define(x: Var, value: Value): this.type = x match
    case x: Global => globals += x -> value; this
    case x: Local  => context.locals += x -> value; this
  def update(rt: RefTarget, value: Value): this.type = rt match {
    case VarTarget(x) => update(x, value); this
    case FieldTarget(base, field) =>
      base match
        // XXX see https://github.com/es-meta/esmeta/issues/65
        case comp: Comp if comp.isAbruptCompletion && field.asStr == "Value" =>
          comp.value = value.toPureValue; this
        case addr: Addr => update(addr, field, value); this
        case _          => error(s"illegal reference update: $rt = $value")
  }
  def update(x: Var, value: Value): this.type =
    x match
      case x: Global if hasBinding(x) => globals += x -> value
      case x: Name if hasBinding(x)   => context.locals += x -> value
      case x: Temp                    => context.locals += x -> value
      case _ => error(s"illegal variable update: $x = $value")
    this
  def update(addr: Addr, field: PureValue, value: Value): this.type =
    heap.update(addr, field, value); this

  /** existence checks */
  private def hasBinding(x: Var): Boolean = x match
    case x: Global => globals contains x
    case x: Local  => context.locals contains x
  def exists(x: Var): Boolean = hasBinding(x) && directLookup(x) != Absent
  def exists(rt: RefTarget): Boolean = rt match {
    case VarTarget(x)             => exists(x)
    case FieldTarget(base, field) => apply(base, field) != Absent
  }

  /** delete a field from a map */
  def delete(rt: RefTarget): this.type = rt match {
    case VarTarget(x) =>
      error(s"cannot delete variable $x")
    case FieldTarget(base, field) =>
      base match {
        case addr: Addr =>
          heap.delete(addr, field); this
        case _ =>
          error(s"illegal reference delete: delete $rt")
      }
  }

  /** object operators */
  def append(addr: Addr, value: Value): this.type =
    heap.append(addr, value); this
  def prepend(addr: Addr, value: Value): this.type =
    heap.prepend(addr, value); this
  def pop(addr: Addr, front: Boolean): Value =
    heap.pop(addr, front)
  def copyObj(addr: Addr): Addr =
    heap.copyObj(addr)
  def keys(addr: Addr, intSorted: Boolean): Addr =
    heap.keys(addr, intSorted)
  def allocRecord(tname: String)(using CFG): Addr =
    heap.allocRecord(tname)
  def allocMap: Addr =
    heap.allocMap
  def allocList(list: List[Value]): Addr =
    heap.allocList(list)
  def allocSymbol(desc: PureValue): Addr =
    heap.allocSymbol(desc)
  def setType(addr: Addr, tname: String): this.type =
    heap.setType(addr, tname); this

  /** get types of values */
  def typeOf(value: Value): ValueTy = value match
    case NormalComp(v) => NormalT(typeOf(v))
    case comp: Comp    => AbruptT(comp.ty.name)
    case addr: Addr =>
      apply(addr) match
        case m: MapObj    => MapT
        case r: RecordObj => NameT(r.tname)
        case l: ListObj   => l.values.map(typeOf).foldLeft(BotT)(_ || _)
        case s: SymbolObj => SymbolT
        case y: YetObj    => NameT(y.tname)
    case clo: Clo      => CloT(clo.func.name)
    case cont: Cont    => ContT(cont.func.id)
    case AstValue(ast) => AstSingleT(ast.name, ast.idx, ast.subIdx)
    case nt: Nt        => NtT(nt)
    case Math(d)       => MathT(d)
    case Infinity(d)   => InfinityT
    case Enum(name)    => EnumT(name)
    case _: CodeUnit   => CodeUnitT
    case _: Number     => NumberT
    case _: BigInt     => BigIntT
    case _: Str        => StrT
    case _: Bool       => BoolT
    case Undef         => UndefT
    case Null          => NullT
    case Absent        => AbsentT

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
