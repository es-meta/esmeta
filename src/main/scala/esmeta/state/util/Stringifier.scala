package esmeta.state.util

import esmeta.cfg.*
import esmeta.state.{*, given}
import esmeta.ir.{Func => IRFunc, *, given}
import esmeta.es.*
import esmeta.util.BaseUtils.*
import esmeta.util.Appender.{given, *}

import esmeta.peval.*
import esmeta.peval.pstate.*

/** stringifier for state elements */
class Stringifier(detail: Boolean, location: Boolean) {
  // load IR Stringifier
  val irStringifier = IRElem.getStringifier((detail, location))
  import irStringifier.{given, *}

  // load CFG Stringifier
  val cfgStringifier = CFGElem.getStringifier((detail, location))
  import cfgStringifier.{given, *}

  // load ECMAScript Stringifier
  val esStringifier = ESElem.getStringifier((false, location, None))
  import esStringifier.{given, *}

  // elements
  given elemRule: Rule[StateElem] = (app, elem) =>
    elem match
      case elem: State       => stRule(app, elem)
      case elem: PState      => pstRule(app, elem)
      case elem: Context     => ctxtRule(app, elem)
      case elem: Cursor      => cursorRule(app, elem)
      case elem: CallContext => callCtxtRule(app, elem)
      case elem: Heap        => heapRule(app, elem)
      case elem: Obj         => objRule(app, elem)
      case elem: Value       => valueRule(app, elem)
      case elem: RefTarget   => refTargetRule(app, elem)
      case elem: Uninit      => uninitRule(app, elem)
      case elem: PHeap       => pheapRule(app, elem)
      case elem: PObj        => pobjRule(app, elem)

  // states
  given stRule: Rule[State] = (app, st) =>
    app.wrap {
      st.filename.map(app :> "filename: " >> _)
      app :> "context: " >> st.context
      given Rule[List[String]] = iterableRule("[", ", ", "]")
      app :> "call-stack: "
      app.wrapIterable("[", ",", "]")(st.callStack)
      app :> "globals: " >> st.globals
      app :> "heap: " >> st.heap
    }

  // pstates
  given pstRule: Rule[PState] = (app, pst) =>
    app.wrap {
      // TODO app :> "globals: " >> pst.globals
      app :> "locals: " >> pst.locals
      app :> "heap: " >> pst.heap
    }

  // predict
  given predictRule: Rule[Predict[Value]] = (app, predict) =>
    predict match
      case Known(value) => app >> value
      case Unknown      => app >> "???"

  // contexts
  given ctxtRule: Rule[Context] = (app, ctxt) =>
    app.wrap {
      app :> "cursor: " >> ctxt.cursor >> " @ " >> ctxt.name
      app :> "local-vars: " >> ctxt.locals
      ctxt.retVal.map(app :> "return: " >> _)
    }

  // cursor
  given cursorRule: Rule[Cursor] = (app, cursor) =>
    cursor match
      case NodeCursor(node) => app >> node.simpleString
      case ExitCursor(func) => app >> func.simpleString

  // calling contexts
  given callCtxtRule: Rule[CallContext] = (app, callCtxt) =>
    val CallContext(context, retId) = callCtxt
    app >> retId >> " @ " >> context.cursor

  // heaps
  given heapRule: Rule[Heap] = (app, heap) =>
    val Heap(map, size) = heap
    app >> s"(SIZE = " >> size.toString >> "): " >> map

  // pheaps
  given pheapRule: Rule[PHeap] = (app, pheap) =>
    val PHeap(map, size) = pheap
    // TODO app >> map
    app >> s"(SIZE = " >> size.toString >> ")"

  // objects
  given objRule: Rule[Obj] = (app, obj) =>
    obj match
      case MapObj(map) =>
        app >> "Map " >> map.map { case (k, v) => (k.toString, v) }
      case RecordObj(tname, map) =>
        app >> "Record"
        given Rule[Iterable[(String, Value | Uninit)]] =
          sortedMapRule("{", "}", " : ")
        if (tname.nonEmpty) app >> "[" >> tname >> "]"
        app >> " " >> map.map { case (k, v) => (s"\"$k\"", v) }
      case ListObj(values) =>
        given Rule[List[Value]] = iterableRule("[", ", ", "]")
        app >> "List" >> values.toList
      case YetObj(tname, msg) =>
        app >> "Yet[" >> tname >> "](\"" >> msg >> "\")"

  // pobjects
  given pobjRule: Rule[PObj] = (app, pobj) => ???
  // obj match
  //   case MapObj(map) =>
  //     app >> "Map " >> map.map { case (k, v) => (k.toString, v) }
  //   case RecordObj(tname, map) =>
  //     app >> "Record"
  //     given Rule[Iterable[(String, Value | Uninit)]] =
  //       sortedMapRule("{", "}", " : ")
  //     if (tname.nonEmpty) app >> "[" >> tname >> "]"
  //     app >> " " >> map.map { case (k, v) => (s"\"$k\"", v) }
  //   case ListObj(values) =>
  //     given Rule[List[Value]] = iterableRule("[", ", ", "]")
  //     app >> "List" >> values.toList
  //   case YetObj(tname, msg) =>
  //     app >> "Yet[" >> tname >> "](\"" >> msg >> "\")"

  // values
  given valueRule: Rule[Value] = (app, value) =>
    value match
      case addr: Addr        => addrRule(app, addr)
      case clo: Clo          => cloRule(app, clo)
      case cont: Cont        => cogrammarSymbolRule(app, cont)
      case AstValue(ast)     => app >> ast
      case gr: GrammarSymbol => grammarSymbolRule(app, gr)
      case m: Math           => mathRule(app, m)
      case i: Infinity       => infinityRule(app, i)
      case e: Enum           => enumRule(app, e)
      case cu: CodeUnit      => cuRule(app, cu)
      case sv: SimpleValue   => svRule(app, sv)

  // addresses
  given addrRule: Rule[Addr] = (app, addr) =>
    addr match
      case NamedAddr(name)   => app >> "#" >> name
      case DynamicAddr(long) => app >> "#" >> long.toString

  // closures
  given cloRule: Rule[Clo] = (app, clo) =>
    val Clo(func, captured) = clo
    given Rule[List[(Name, Value)]] = iterableRule("[", ", ", "]")
    app >> "clo<" >> func.irFunc.name
    if (!captured.isEmpty) app >> ", " >> captured.toList
    app >> ">"

  // continuations
  given cogrammarSymbolRule: Rule[Cont] = (app, cont) =>
    val Cont(func, captured, _) = cont
    given Rule[List[(Name, Value)]] = iterableRule("[", ", ", "]")
    app >> "cont<" >> func.irFunc.name
    if (!captured.isEmpty) app >> ", " >> captured.toList
    app >> ">"

  // nonterminals
  given grammarSymbolRule: Rule[GrammarSymbol] = (app, gr) =>
    given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
    given Rule[List[Boolean]] = iterableRule()
    app >> "|" >> gr.name >> "|"
    if (!gr.params.isEmpty) app >> "[" >> gr.params >> "]"
    app

  // math
  given mathRule: Rule[Math] = (app, math) => app >> math.decimal

  // infinity
  given infinityRule: Rule[Infinity] = (app, inf) =>
    app >> (if (inf.pos) "+INF" else "-INF")

  // enum
  given enumRule: Rule[Enum] = (app, e) => app >> "~" >> e.name >> "~"

  // code unit
  given cuRule: Rule[CodeUnit] = (app, cu) => app >> cu.c.toInt >> "cu"

  // simple values
  given svRule: Rule[SimpleValue] = (app, sv) =>
    sv match
      case Number(n)  => app >> toStringHelper(n)
      case BigInt(n)  => app >> n >> "n"
      case Str(str)   => app >> "\"" >> str >> "\""
      case Bool(bool) => app >> bool
      case Undef      => app >> "undefined"
      case Null       => app >> "null"

  // reference value
  lazy val inlineField = "([_a-zA-Z][_a-zA-Z0-9]*)".r
  given refTargetRule: Rule[RefTarget] = (app, refTarget) =>
    refTarget match {
      case VarTarget(id)                            => app >> id
      case FieldTarget(base, Str(inlineField(str))) => app >> base >> "." >> str
      case FieldTarget(base, field) => app >> base >> "[" >> field >> "]"
    }

  // uninit
  given uninitRule: Rule[Uninit] = (app, _) => app >> "uninit"
}
