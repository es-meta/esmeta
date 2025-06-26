package esmeta.ty

import esmeta.ESMetaTest
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.state.*
import scala.collection.mutable.{Map => MMap, LinkedHashMap => LMMap}
import esmeta.es.*

/** contains test */
class ContainsTinyTest extends TyTest {
  val name: String = "tyContainsTest"

  given CFG = ESMetaTest.cfg

  case class Neg(name: String, heap: Heap) {
    def neg(ps: (Ty, Value)*): Unit =
      given Heap = heap
      checkContains(name, expected = false)(ps*)
  }

  // check containment of values for types
  def checkContains(name: String, expected: Boolean = true)(using heap: Heap)(
    ps: (Ty, Value)*,
  ): Neg =
    val desc = name + (if (expected) "" else " (negative)")
    check(desc)(for {
      (ty, value) <- ps
      if expected != ty.contains(value, heap)
    } {
      println(s"[FAILED] $desc")
      fail(s"$ty contains $value should be $expected")
    })
    Neg(name, heap)

  // registration
  def init: Unit = {
    // pre-defined CFG function
    lazy val func = Func(
      0,
      IRFunc(true, IRFuncKind.AbsOp, "f", Nil, UnknownType, INop()),
      Block(0),
    )

    // pre-defined heap
    lazy val mapAddr = NamedAddr("mapAddr")
    lazy val mapObj = MapObj(LMMap(Number(42) -> Undef))
    lazy val recordAddr = NamedAddr("recordAddr")
    lazy val recordObj = RecordObj("", MMap("P" -> Number(42)))
    lazy val nilAddr = NamedAddr("nilAddr")
    lazy val nilObj = ListObj(Vector())
    lazy val listAddr = NamedAddr("listAddr")
    lazy val listObj = ListObj(Vector(Math(5)))
    lazy val symbolAddr = NamedAddr("symbolAddr")
    lazy val symbolObj = RecordObj("Symbol", MMap("Description" -> Str("desc")))
    given Heap = Heap(
      MMap(
        mapAddr -> mapObj,
        recordAddr -> recordObj,
        nilAddr -> nilObj,
        listAddr -> listObj,
        symbolAddr -> symbolObj,
      ),
    )

    checkContains("map objects")(
      MapT -> mapAddr,
      MapT(NumberT, UndefT) -> mapAddr,
      MapT(AnyT, AnyT) -> mapAddr,
    ).neg(
      MapT(StrT, UndefT) -> mapAddr,
      MapT(NumberT, StrT) -> mapAddr,
    )
    checkContains("record objects")(
      RecordT -> recordAddr,
      RecordT("") -> recordAddr,
      RecordT("", Map("P" -> NumberT)) -> recordAddr,
    ).neg(
      RecordT("B") -> recordAddr,
      RecordT("", Map("P" -> AnyT, "Q" -> AnyT)) -> recordAddr,
      RecordT("", Map("P" -> BoolT)) -> recordAddr,
    )

    checkContains("list objects")(
      ListT -> nilAddr,
      ListT -> listAddr,
      NilT -> nilAddr,
      ListT(MathT) -> nilAddr,
      ListT(MathT) -> listAddr,
    ).neg(
      NilT -> listAddr,
      ListT(StrT) -> listAddr,
    )

    checkContains("symbol objects")(
      SymbolT -> symbolAddr,
    ).neg(
      SymbolT -> nilAddr,
    )

    lazy val clo = Clo(func, Map())
    checkContains("closures")(
      CloT -> clo,
      CloT("f") -> clo,
    ).neg(
      CloT("g") -> clo,
    )

    lazy val cont = Cont(func, Map(), Nil)
    checkContains("continuations")(
      ContT -> cont,
      ContT(0) -> cont,
    ).neg(
      ContT(42) -> cont,
    )

    lazy val F = false
    lazy val T = true
    lazy val ast1 = Syntactic("A", List(F, T), 5, Vector.empty)
    lazy val astValue1 = AstValue(ast1)
    lazy val ast2 = Syntactic("B", List(F, T), 5, Vector(Some(ast1), None))
    lazy val astValue2 = AstValue(ast2)
    checkContains("abstract syntax tree (AST) values")(
      AstT -> astValue1,
      AstT("A") -> astValue1,
      AstT("B", 5) -> astValue2,
    ).neg(
      AstT("B") -> astValue1,
      AstT("A", 5) -> astValue2,
      AstT("B", 3) -> astValue2,
    )

    lazy val grammarSymbolA = GrammarSymbol("A", List(T, F, T))
    lazy val grammarSymbolB = GrammarSymbol("B", Nil)
    checkContains("nonterminals")(
      GrammarSymbolT -> grammarSymbolA,
      GrammarSymbolT(grammarSymbolA) -> grammarSymbolA,
    ).neg(
      GrammarSymbolT(grammarSymbolB) -> grammarSymbolA,
    )

    checkContains("math values")(
      MathT -> Math(52.24),
      MathT(52.24) -> Math(52.24),
    ).neg(
      MathT(52.24) -> Math(1.5),
    )

    checkContains("enum values")(
      EnumT -> Enum("empty"),
      EnumT("empty") -> Enum("empty"),
    ).neg(
      EnumT("empty") -> Enum("iterate"),
    )

    checkContains("code units")(
      CodeUnitT -> CodeUnit(' '),
    )

    checkContains("numeric values")(
      NumberT -> Number(42),
      NumberT(Number(1.2)) -> Number(1.2),
      BigIntT -> BigInt(42),
    ).neg(
      NumberT(Number(1.2)) -> Number(5.7),
    )

    checkContains("non-numeric simple values")(
      StrT -> Str("test"),
      StrT("a", "b") -> Str("a"),
      BoolT -> Bool(F),
      BoolT(T) -> Bool(T),
      UndefT -> Undef,
      NullT -> Null,
    ).neg(
      StrT("a") -> Str("b"),
      BoolT(T) -> Bool(F),
    )
  }
  init
}
