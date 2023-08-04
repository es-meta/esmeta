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
  def directLookup(x: Id): Value = (x match {
    case x: Global => globals.get(x)
    case x: Local  => context.locals.get(x)
  }).getOrElse(throw UnknownId(x))

  /** getters */
  def apply(refV: RefValue): Value = refV match
    case IdValue(x)            => apply(x)
    case PropValue(base, prop) => apply(base, prop)
  def apply(x: Id): Value = directLookup(x) match
    case Absent if func.isBuiltin => Undef
    case v                        => v
  def apply(base: Value, prop: PureValue): Value = base match
    case comp: Comp =>
      prop match
        case Str("Type")   => comp.ty
        case Str("Value")  => comp.value
        case Str("Target") => comp.targetValue
        case _             => throw InvalidCompProp(comp, prop)
    case addr: Addr    => heap(addr, prop)
    case AstValue(ast) => apply(ast, prop)
    case Str(str)      => apply(str, prop)
    case v             => throw InvalidRefBase(v)
  def apply(ast: Ast, prop: PureValue): PureValue =
    (ast, prop) match
      case (_, Str("parent")) => ast.parent.map(AstValue(_)).getOrElse(Absent)
      case (syn: Syntactic, Str(propStr)) =>
        val Syntactic(name, _, rhsIdx, children) = syn
        val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
        rhs.getNtIndex(propStr).flatMap(children(_)) match
          case Some(child) => AstValue(child)
          case _           => throw InvalidAstProp(syn, Str(propStr))
      case (syn: Syntactic, Math(n)) if n.isValidInt =>
        syn.children(n.toInt).map(AstValue(_)).getOrElse(Absent)
      case _ => throw InvalidAstProp(ast, prop)
  def apply(str: String, prop: PureValue): PureValue = prop match
    case Str("length") => Math(BigDecimal(str.length))
    case Math(k)       => CodeUnit(str(k.toInt))
    case Number(k)     => CodeUnit(str(k.toInt))
    case _             => throw WrongStringRef(str, prop)
  def apply(addr: Addr): Obj = heap(addr)

  /** setters */
  def define(x: Id, value: Value): this.type = x match
    case x: Global => globals += x -> value; this
    case x: Local  => context.locals += x -> value; this
  def update(refV: RefValue, value: Value): this.type = refV match {
    case IdValue(x) => update(x, value); this
    case PropValue(base, prop) =>
      base match
        // XXX see https://github.com/es-meta/esmeta/issues/65
        case comp: Comp if comp.isAbruptCompletion && prop.asStr == "Value" =>
          comp.value = value.toPureValue; this
        case addr: Addr => update(addr, prop, value); this
        case _          => error(s"illegal reference update: $refV = $value")
  }
  def update(x: Id, value: Value): this.type =
    x match
      case x: Global if hasBinding(x) => globals += x -> value
      case x: Name if hasBinding(x)   => context.locals += x -> value
      case x: Temp                    => context.locals += x -> value
      case _ => error(s"illegal variable update: $x = $value")
    this
  def update(addr: Addr, prop: PureValue, value: Value): this.type =
    heap.update(addr, prop, value); this

  /** existence checks */
  private def hasBinding(x: Id): Boolean = x match
    case x: Global => globals contains x
    case x: Local  => context.locals contains x
  def exists(x: Id): Boolean = hasBinding(x) && directLookup(x) != Absent
  def exists(ref: RefValue): Boolean = ref match {
    case IdValue(id)           => exists(id)
    case PropValue(base, prop) => apply(base, prop) != Absent
  }

  /** delete a property from a map */
  def delete(refV: RefValue): this.type = refV match {
    case IdValue(x) =>
      error(s"cannot delete variable $x")
    case PropValue(base, prop) =>
      base match {
        case addr: Addr =>
          heap.delete(addr, prop); this
        case _ =>
          error(s"illegal reference delete: delete $refV")
      }
  }

  /** object operators */
  def append(addr: Addr, value: PureValue): this.type =
    heap.append(addr, value); this
  def prepend(addr: Addr, value: PureValue): this.type =
    heap.prepend(addr, value); this
  def pop(addr: Addr, front: Boolean): PureValue =
    heap.pop(addr, front)
  def remove(addr: Addr, value: PureValue): this.type =
    heap.remove(addr, value); this
  def copyObj(addr: Addr): Addr =
    heap.copyObj(addr)
  def keys(addr: Addr, intSorted: Boolean): Addr =
    heap.keys(addr, intSorted)
  def allocMap(tname: String, map: Map[PureValue, PureValue] = Map())(using
    CFG,
  ): Addr = heap.allocMap(tname, map)
  def allocList(list: List[PureValue]): Addr =
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
        case m: MapObj    => NameT(m.ty)
        case l: ListObj   => l.values.map(typeOf).foldLeft(BotT)(_ || _)
        case s: SymbolObj => SymbolT
        case y: YetObj    => NameT(y.tname)
    case clo: Clo      => CloT(clo.func.name)
    case cont: Cont    => ContT(cont.func.id)
    case AstValue(ast) => AstSingleT(ast.name, ast.idx, ast.subIdx)
    case nt: Nt        => NtT(nt)
    case Math(d)       => MathT(d)
    case MathInf(pos)  => MathInfT(pos)
    case Const(name)   => ConstT(name)
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
