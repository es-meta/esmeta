package esmeta.interp

import esmeta.cfg.*
import esmeta.error.*
import esmeta.interp.util.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}

/** IR states */
case class State(
  val cfg: CFG,
  var context: Context,
  var callStack: List[CallContext] = Nil,
  val globals: MMap[Global, Value] = MMap(),
  val heap: Heap = Heap(),
) extends InterpElem {

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
        case _             => apply(comp.escaped, prop)
    case addr: Addr    => heap(addr, prop)
    case AstValue(ast) => apply(ast, prop)
    case Str(str)      => apply(str, prop)
    case v             => throw InvalidRefBase(v)
  def apply(ast: Ast, prop: PureValue): PureValue =
    ast match
      case syn: Syntactic =>
        prop match
          // access to SDO
          case Str(propStr) if ast.isInstanceOf[Syntactic] =>
            cfg.getSDO((ast, propStr)) match
              case Some((ast0, sdo)) =>
                Clo(sdo, Map(NAME_THIS -> AstValue(ast0)))
              case None => throw InvalidAstProp(ast, prop)
          // access to child
          case Math(n) if n.isValidInt =>
            ast.getChildren(n.toInt) match
              case Some(child) => AstValue(child)
              case None        => Absent
          case _ => throw InvalidAstProp(ast, prop)
      case lex: Lexical =>
        prop match
          // get string value of lexical
          case Str("StringValue") => Str(lex.str)
          case _                  => throw InvalidAstProp(ast, prop)
  def apply(str: String, prop: PureValue): PureValue = prop match
    case Str("length") => Math(str.length)
    case Math(k)       => Str(str(k.toInt).toString)
    case Number(k)     => Str(str(k.toInt).toString)
    case _             => throw WrongStringRef(str, prop)
  def apply(addr: Addr): Obj = heap(addr)

  /** setters */
  def define(x: Id, value: Value): this.type = x match
    case x: Global => globals += x -> value; this
    case x: Local  => context.locals += x -> value; this
  def update(refV: RefValue, value: Value): this.type = refV match {
    case IdValue(x) => update(x, value); this
    case PropValue(base, prop) =>
      base.escaped match {
        case addr: Addr => update(addr, prop, value); this
        case _          => error(s"illegal reference update: $refV = $value")
      }
  }
  def update(x: Id, value: Value): this.type =
    x match
      case x: Global if exists(x) => globals += x -> value
      case x: Name if exists(x)   => context.locals += x -> value
      case x: Temp                => context.locals += x -> value
      case _ => error(s"illegal variable update: $x = $value")
    this
  def update(addr: Addr, prop: PureValue, value: Value): this.type =
    heap.update(addr, prop, value); this

  /** existence checks */
  def exists(x: Id): Boolean = (x match {
    case x: Global => globals contains x
    case x: Local  => context.locals contains x
  }) && directLookup(x) != Absent
  def exists(ref: RefValue): Boolean = ref match {
    case IdValue(id)           => exists(id)
    case PropValue(base, prop) => apply(base.escaped, prop) != Absent
  }

  /** delete a property from a map */
  def delete(refV: RefValue): this.type = refV match {
    case IdValue(x) =>
      error(s"cannot delete variable $x")
    case PropValue(base, prop) =>
      base.escaped match {
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
}
object State {

  /** initialize states with a CFG */
  def apply(cfg: CFG): State = State(cfg, Context(cfg.main))
}
