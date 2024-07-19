package esmeta.state

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.es.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap, ListBuffer}

/** stringify test */
class StringifyTinyTest extends StateTest {
  val name: String = "stateStringifyTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // States
    // -------------------------------------------------------------------------
    lazy val st = State(CFG(List(func)), ctxt)
    checkStringify("State")(
      st -> """{
      |  context: {
      |    cursor: Block[0] @ f
      |    local-vars: {}
      |  }
      |  call-stack: []
      |  globals: {}
      |  heap: (SIZE = 0): {}
      |}""".stripMargin,
    )
    // -------------------------------------------------------------------------
    // Calling Contexts
    // -------------------------------------------------------------------------
    lazy val callCtxt = CallContext(Name("x"), ctxt)
    checkStringify("CallContext")(
      callCtxt -> "x @ Block[0]",
    )
    // -------------------------------------------------------------------------
    // Contexts
    // -------------------------------------------------------------------------
    lazy val ctxt = Context(func, MMap())
    lazy val ctxtSingle = Context(func, MMap(Name("x") -> Math(42)))
    lazy val ctxtMulti =
      Context(func, MMap(Name("x") -> Math(42), Name("y") -> Str("abc")))
    checkStringify("Context")(
      ctxt -> """{
      |  cursor: Block[0] @ f
      |  local-vars: {}
      |}""".stripMargin,
      ctxtSingle -> """{
      |  cursor: Block[0] @ f
      |  local-vars: {
      |    x -> 42
      |  }
      |}""".stripMargin,
      ctxtMulti -> """{
      |  cursor: Block[0] @ f
      |  local-vars: {
      |    x -> 42
      |    y -> "abc"
      |  }
      |}""".stripMargin,
    )
    // -------------------------------------------------------------------------
    // Cursor
    // -------------------------------------------------------------------------
    lazy val nodeCursor = NodeCursor(Block(3, ListBuffer()))
    lazy val exitCursor = ExitCursor(func)
    checkStringify("Cursor")(
      nodeCursor -> "Block[3]",
      exitCursor -> "Func[0]",
    )
    // -------------------------------------------------------------------------
    // Heaps
    // -------------------------------------------------------------------------
    lazy val heap = Heap(MMap(), 0)
    lazy val heapSingle = Heap(MMap(namedAddr -> map), 0)
    lazy val heapMulti = Heap(MMap(namedAddr -> symbol, addr -> list), 43)
    checkStringify("Heap")(
      heap -> "(SIZE = 0): {}",
      heapSingle -> """(SIZE = 0): {
      |  #Global -> [TYPE = A] {}
      |}""".stripMargin,
      heapMulti -> """(SIZE = 43): {
      |  #Global -> (Symbol "description")
      |  #42 -> [42, "x"]
      |}""".stripMargin,
    )
    // -------------------------------------------------------------------------
    // Objects
    // -------------------------------------------------------------------------
    lazy val map = MapObj("A", MMap(), 0)
    lazy val singleMap =
      MapObj("A", MMap(Str("p") -> MapObj.Prop(Str("p"), 0)), 1)
    lazy val list = ListObj(Vector(Math(42), Str("x")))
    lazy val symbol = SymbolObj(Str("description"))
    lazy val yet = YetObj("A", "message")
    checkStringify("Object")(
      map -> "[TYPE = A] {}",
      singleMap -> """[TYPE = A] {
      |  "p" -> "p"
      |}""".stripMargin,
      list -> """[42, "x"]""",
      symbol -> """(Symbol "description")""",
      yet -> """(Yet [TYPE = A] "message")""",
    )
    // -------------------------------------------------------------------------
    // Values
    // -------------------------------------------------------------------------
    lazy val normalComp = Comp(Enum("normal"), Math(42), None)
    lazy val comp = Comp(Enum("throw"), Math(42), Some("to"))
    lazy val namedAddr = NamedAddr("Global")
    lazy val addr = DynamicAddr(42)
    lazy val func =
      Func(
        0,
        IRFunc(true, IRFuncKind.AbsOp, "f", Nil, UnknownType, INop()),
        Block(0),
      )
    lazy val clo = Clo(func, Map())
    lazy val cloCaptured = Clo(func, Map(Name("x") -> Str("abc")))
    lazy val cont = Cont(func, Map(), Nil)
    lazy val contCaptured = Cont(func, Map(Name("x") -> Str("abc")), Nil)
    lazy val ast = AstValue(Syntactic("Identifier", Nil, 1, Nil))
    lazy val astArgs =
      AstValue(Syntactic("Identifier", List(true, false), 1, Nil))
    lazy val nt = Nt("Identifier", List(true, false))
    lazy val lex = AstValue(Lexical("Identifier", "x"))
    checkStringify("Value")(
      comp -> "comp[~throw~/to](42)",
      normalComp -> "N(42)",
      namedAddr -> "#Global",
      addr -> "#42",
      clo -> "clo<f>",
      cloCaptured -> """clo<f, [x -> "abc"]>""",
      cont -> "cont<f>",
      contCaptured -> """cont<f, [x -> "abc"]>""",
      ast -> "|Identifier|<1>",
      astArgs -> "|Identifier|[TF]<1>",
      lex -> "|Identifier|(x)",
      nt -> "|Identifier|[TF]",
      Math(3.2) -> "3.2",
      Number(3.2) -> "3.2",
      BigInt(324) -> "324n",
      Str("abc") -> "\"abc\"",
      Bool(true) -> "true",
      Bool(false) -> "false",
      Undef -> "undefined",
      Null -> "null",
      Absent -> "absent",
      Enum("empty") -> "~empty~",
      CodeUnit(97) -> "97cu",
    )
    // -------------------------------------------------------------------------
    // Reference Values
    // -------------------------------------------------------------------------
    lazy val x = Name("x")
    lazy val xRefVal = IdValue(x)
    lazy val prop = PropValue(addr, Str("prop"))
    lazy val noInlineProp = PropValue(addr, Math(42))
    checkStringify("RefValue")(
      xRefVal -> "x",
      prop -> "#42.prop",
      noInlineProp -> "#42[42]",
    )
  }

  init
}
