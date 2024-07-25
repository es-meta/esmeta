package esmeta.ty

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.state.*
import scala.collection.mutable.{Map => MMap, LinkedHashMap => LMMap}
import esmeta.es.*

/** contains test */
class ContainsTinyTest extends TyTest {
  val name: String = "tyContainsTest"

  case class Neg(name: String, heap: Heap) {
    def neg(ps: (Ty, Value)*): Unit =
      given Heap = heap
      checkContains(name, expected = false)(ps: _*)
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
    lazy val mapObj = MapObj(LMMap())
    lazy val recordAddr = NamedAddr("recordAddr")
    lazy val recordObj = RecordObj("Record", MMap("P" -> Number(42)))
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

    lazy val normalComp = NormalComp(Str("a"))
    lazy val throwComp = Comp(Enum("throw"), Str("a"), None)
    lazy val returnComp = Comp(Enum("return"), Str("a"), None)
    checkContains("completion values")(
      NormalT -> normalComp,
      NormalT(StrT) -> normalComp,
      AbruptT -> throwComp,
      AbruptT("throw") -> throwComp,
    ).neg(
      NormalT -> throwComp,
      NormalT(NumberT) -> normalComp,
      AbruptT -> normalComp,
      AbruptT("throw") -> returnComp,
    )

    checkContains("map objects")(
      NameT -> mapAddr,
      NameT("A") -> mapAddr,
      RecordT -> recordAddr,
      RecordT(Set("P")) -> recordAddr,
      RecordT("P" -> NumberT) -> recordAddr,
    ).neg(
      NameT("B") -> recordAddr,
      RecordT(Set("Q")) -> recordAddr,
      RecordT("P" -> BoolT) -> recordAddr,
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

    lazy val ast1 = Syntactic("A", List(false, true), 5, Nil)
    lazy val astValue1 = AstValue(ast1)
    lazy val ast2 = Syntactic("B", List(false, true), 5, List(Some(ast1), None))
    lazy val astValue2 = AstValue(ast2)
    checkContains("abstract syntax tree (AST) values")(
      AstT -> astValue1,
      AstT("A") -> astValue1,
      AstSingleT("B", 5, 2) -> astValue2,
    ).neg(
      AstT("B") -> astValue1,
      AstSingleT("A", 5, 2) -> astValue2,
      AstSingleT("B", 3, 2) -> astValue2,
      AstSingleT("B", 5, 1) -> astValue2,
    )

    lazy val ntA = Nt("A", List(true, false, true))
    lazy val ntB = Nt("B", Nil)
    checkContains("nonterminals")(
      NtT -> ntA,
      NtT(ntA) -> ntA,
    ).neg(
      NtT(ntB) -> ntA,
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
      BoolT -> Bool(false),
      BoolT(true) -> Bool(true),
      UndefT -> Undef,
      NullT -> Null,
      AbsentT -> Absent,
    ).neg(
      StrT("a") -> Str("b"),
      BoolT(true) -> Bool(false),
    )
  }
  init
}
