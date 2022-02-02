package esmeta.interp.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.cfg.util.*
import esmeta.interp.*
import esmeta.util.*
import esmeta.util.Appender.{given, *}
import esmeta.util.BaseUtils.*

/** stringifier for Interp */
class Stringifier(detail: Boolean, location: Boolean) {
  // load CFG Stringifier
  val cfgStringifier = CFGElem.getStringifier((detail, location))
  import cfgStringifier.{given, *}

  // elements
  given elemRule: Rule[InterpElem] = (app, elem) =>
    elem match
      case elem: State       => stRule(app, elem)
      case elem: Context     => ctxtRule(app, elem)
      case elem: Cursor      => cursorRule(app, elem)
      case elem: CallContext => callCtxtRule(app, elem)
      case elem: Heap        => heapRule(app, elem)
      case elem: Obj         => objRule(app, elem)
      case elem: Value       => valueRule(app, elem)
      case elem: RefValue    => refValRule(app, elem)

  // states
  given stRule: Rule[State] = (app, st) =>
    app.wrap {
      app :> "context: " >> st.context
      given Rule[List[String]] = iterableRule("[", ", ", "]")
      app :> "call-stack: "
      app.wrapIterable("[", "]")(st.globals)
      app :> "globals: "
      app.wrapIterable(st.globals)
      app :> "heap: " >> st.heap
    }

  // contexts
  given ctxtRule: Rule[Context] = (app, ctxt) =>
    app.wrap {
      app :> "cursor: " >> ctxt.cursor >> " @ " >> ctxt.func.name
      app :> "local-vars: "
      app.wrapIterable(ctxt.locals)
      ctxt.retVal.map(app :> "return: " >> _)
    }

  // cursor
  given cursorRule: Rule[Cursor] = (app, cursor) =>
    cursor match
      case NodeCursor(node) => app >> node.simpleString
      case ExitCursor(func) => app >> func.simpleString

  // calling contexts
  given callCtxtRule: Rule[CallContext] = (app, callCtxt) =>
    val CallContext(retId, context) = callCtxt
    app >> retId >> " @ " >> context.cursor

  // heaps
  given heapRule: Rule[Heap] = (app, heap) =>
    val Heap(map, size) = heap
    app >> s"(SIZE = " >> size.toString >> "): "
    app.wrapIterable(map)

  // objects
  given objRule: Rule[Obj] = (app, obj) =>
    obj match
      case map @ MapObj(tname, _, _) =>
        app >> "[TYPE = " >> tname >> "] "
        app.wrapIterable(map.pairs)
      case ListObj(values) =>
        given Rule[List[Value]] = iterableRule("[", ", ", "]")
        app >> values.toList
      case SymbolObj(desc) => app >> "(Symbol " >> desc >> ")"
      case YetObj(tname, msg) =>
        app >> "(Yet [TYPE = " >> tname >> "] \"" >> msg >> "\")"

  // values
  given valueRule: Rule[Value] = (app, value) =>
    value match
      case comp: Comp      => compRule(app, comp)
      case pure: PureValue => pureValueRule(app, pure)

  // completion values
  given compRule: Rule[Comp] = (app, comp) =>
    comp match
      case NormalComp(value) =>
        app >> "N(" >> value >> ")"
      case Comp(ty, value, target) =>
        app >> "comp[" >> ty
        target.map(app >> "/" >> _)
        app >> "]" >> "(" >> value >> ")"

  // pure values (not completion values)
  given pureValueRule: Rule[PureValue] = (app, value) =>
    value match
      case addr: Addr        => addrRule(app, addr)
      case clo: Clo          => cloRule(app, clo)
      case cont: Cont        => contRule(app, cont)
      case ast: Ast          => astRule(app, ast)
      case lit: LiteralValue => litRule(app, lit)

  // addresses
  given addrRule: Rule[Addr] = (app, addr) =>
    addr match
      case NamedAddr(name)   => app >> "#" >> name
      case DynamicAddr(long) => app >> "#" >> long.toString

  // closures
  given cloRule: Rule[Clo] = (app, clo) =>
    val Clo(func, captured) = clo
    given Rule[List[(Name, Value)]] = iterableRule("[", ", ", "]")
    app >> "clo<" >> func.name
    if (!captured.isEmpty) app >> ", " >> captured.toList
    app >> ">"

  // continuations
  given contRule: Rule[Cont] = (app, cont) =>
    val Cont(func, captured, _) = cont
    given Rule[List[(Name, Value)]] = iterableRule("[", ", ", "]")
    app >> "cont<" >> func.name
    if (!captured.isEmpty) app >> ", " >> captured.toList
    app >> ">"

  // abstract syntax tree (AST) values
  given astRule: Rule[Ast] = (app, ast) =>
    ast match
      case Syntactic(name, args, rhsIdx, bits, _) =>
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule()
        app >> "|" >> name >> "|"
        if (!args.isEmpty) app >> "[" >> args >> "]"
        app >> "<" >> rhsIdx >> ", " >> bits >> ">"
      case Lexical(name, str) =>
        app >> "|" >> name >> "|(" >> str >> ")"

  // literal values
  given litRule: Rule[LiteralValue] = (app, lit) =>
    lit match
      case Math(n)     => app >> n
      case Number(n)   => app >> n >> "f"
      case BigInt(n)   => app >> n >> "n"
      case Str(str)    => app >> "\"" >> str >> "\""
      case Bool(bool)  => app >> bool
      case Undef       => app >> "undefined"
      case Null        => app >> "null"
      case Absent      => app >> "absent"
      case Const(name) => app >> "~" >> name >> "~"

  // reference value
  lazy val inlineProp = "([_a-zA-Z][_a-zA-Z0-9]*)".r
  given refValRule: Rule[RefValue] = (app, refValue) =>
    refValue match {
      case IdValue(id)                           => app >> id
      case PropValue(base, Str(inlineProp(str))) => app >> base >> "." >> str
      case PropValue(base, prop) => app >> base >> "[" >> prop >> "]"
    }
}
