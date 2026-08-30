package esmeta.ty

import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.state.*
import esmeta.util.*
import scala.collection.mutable.{Map => MMap, LinkedHashMap => LMMap}

/** lattice test for the non-numeric type domains
  *
  * The numeric domains are covered by `NumericTinyTest`; this covers the rest,
  * where the concrete witnesses are objects on a heap rather than numbers.
  */
class LatticeTinyTest extends TyTest {
  val name: String = "tyLatticeTest"

  // ---------------------------------------------------------------------------
  // concrete witnesses
  // ---------------------------------------------------------------------------
  private val F = false
  private val T = true

  private val func = Func(
    0,
    IRFunc(true, IRFuncKind.AbsOp, "f", Nil, UnknownType, INop()),
    Block(0),
  )

  // lists, including the empty one, which belongs to every element type
  private val nilAddr = NamedAddr("nil")
  private val mathListAddr = NamedAddr("mathList")
  private val strListAddr = NamedAddr("strList")
  private val mixedListAddr = NamedAddr("mixedList")

  // maps, including the empty one, which belongs to every key/value type
  private val emptyMapAddr = NamedAddr("emptyMap")
  private val numMapAddr = NamedAddr("numMap")
  private val strMapAddr = NamedAddr("strMap")

  // records
  private val recordAddr = NamedAddr("record")
  private val symbolAddr = NamedAddr("symbol")
  private val normalAddr = NamedAddr("normal")
  private val abruptAddr = NamedAddr("abrupt")
  private val completionAddr = NamedAddr("completion")

  private val heap: Heap = Heap(
    MMap(
      nilAddr -> ListObj(Vector()),
      mathListAddr -> ListObj(Vector(Math(5))),
      strListAddr -> ListObj(Vector(Str("a"))),
      mixedListAddr -> ListObj(Vector(Math(5), Str("a"))),
      emptyMapAddr -> MapObj(LMMap()),
      numMapAddr -> MapObj(LMMap(Number(42) -> Undef)),
      strMapAddr -> MapObj(LMMap(Str("k") -> Str("v"))),
      recordAddr -> RecordObj("", MMap("P" -> Number(42))),
      symbolAddr -> RecordObj("Symbol", MMap("Description" -> Str("desc"))),
      normalAddr -> RecordObj(
        "NormalCompletion",
        MMap(
          "Type" -> Enum("normal"),
          "Value" -> Undef,
          "Target" -> Enum("empty"),
        ),
      ),
      abruptAddr -> RecordObj(
        "AbruptCompletion",
        MMap(
          "Type" -> Enum("throw"),
          "Value" -> Undef,
          "Target" -> Enum("empty"),
        ),
      ),
      completionAddr -> RecordObj(
        "CompletionRecord",
        MMap(
          "Type" -> Enum("normal"),
          "Value" -> Undef,
          "Target" -> Enum("empty"),
        ),
      ),
    ),
  )

  private def listObj(a: Addr): ListObj =
    heap(a).asInstanceOf[ListObj]
  private def mapObj(a: Addr): MapObj =
    heap(a).asInstanceOf[MapObj]

  private def recordObj(a: Addr): RecordObj =
    heap(a).asInstanceOf[RecordObj]

  private val recordAddrs =
    List(recordAddr, symbolAddr, normalAddr, abruptAddr, completionAddr)

  private val listAddrs =
    List(nilAddr, mathListAddr, strListAddr, mixedListAddr)
  private val mapAddrs = List(emptyMapAddr, numMapAddr, strMapAddr)

  private val astA0 = Syntactic("A", List(F, T), 0, Vector.empty)
  private val astA1 = Syntactic("A", List(F, T), 1, Vector.empty)
  private val astB5 = Syntactic("B", List(F, T), 5, Vector(Some(astA0), None))
  private val asts = List(astA0, astA1, astB5).map(AstValue(_))

  private val strs = List("a", "b", "c")
  private val bools = List(true, false)

  // ---------------------------------------------------------------------------
  // type universes, including the redundant encodings of top and bottom
  // ---------------------------------------------------------------------------
  private val listTys: List[ListTy] = List(
    ListTy.Top,
    ListTy.Bot,
    ListTy.Nil,
    ListTy.Elem(MathT),
    ListTy.Elem(StrT),
    ListTy.Elem(MathT || StrT),
    // redundant encoding
    ListTy.Elem(AnyT),
  )

  private val mapTys: List[MapTy] = List(
    MapTy.Top,
    MapTy.Bot,
    MapTy.Empty,
    MapTy(NumberT, UndefT),
    MapTy(StrT, StrT),
    MapTy(NumberT || StrT, UndefT || StrT),
    // redundant encodings
    MapTy.Elem(AnyT, AnyT),
    MapTy(NumberT, BotT),
  )

  private val astTys: List[AstTy] = List(
    AstTy.Top,
    AstTy.Bot,
    AstTy.Simple(Set("A")),
    AstTy.Simple(Set("B")),
    AstTy.Simple(Set("A", "B")),
    AstTy.Detail("A", 0),
    AstTy.Detail("A", 1),
    AstTy.Detail("B", 5),
  )

  private val boolTys: List[BoolTy] = List(
    BoolTy.Top,
    BoolTy.Bot,
    BoolTy(true),
    BoolTy(false),
  )

  private val strTys: List[BSet[String]] = List(
    Inf,
    Fin[String](),
    Fin("a"),
    Fin("b"),
    Fin("a", "b"),
  )

  private val cloTys: List[CloTy] = List(
    CloTy.Top,
    CloTy.Bot,
    CloSetTy(Set("f")),
    CloSetTy(Set("g")),
    CloSetTy(Set("f", "g")),
  )

  private val cloNames = List("f", "g", "h")

  /** real declarations from the type model, including a parent and two of its
    * children, so that subtyping through `TyModel` is exercised
    */
  private val recordTys: List[RecordTy] = List(
    RecordTy.Top,
    RecordTy.Bot,
    RecordTy("CompletionRecord"),
    RecordTy("NormalCompletion"),
    RecordTy("AbruptCompletion"),
    RecordTy("Symbol"),
    RecordTy(""),
    // refined by a field
    RecordTy("CompletionRecord", Map("Type" -> EnumT("normal"))),
    RecordTy("CompletionRecord", Map("Type" -> EnumT("throw"))),
    RecordTy("NormalCompletion", Map("Type" -> EnumT("normal"))),
    RecordTy("Symbol", Map("Description" -> StrT)),
    // a union of a parent and a child
    RecordTy("NormalCompletion") || RecordTy("AbruptCompletion"),
    RecordTy("CompletionRecord") || RecordTy("Symbol"),
  )

  /** one witness per `ValueTy` component, to check that the component-wise
    * composition really is exact -- it is only sound if the components are
    * pairwise disjoint sets of concrete values
    */
  private val values: List[Value] = List(
    nilAddr,
    mathListAddr,
    emptyMapAddr,
    numMapAddr,
    recordAddr,
    symbolAddr,
    Clo(func, Map()),
    Cont(func, Map(), Nil),
    AstValue(astA0),
    AstValue(astB5),
    GrammarSymbol("A", List(T, F)),
    Math(5),
    Infinity(true),
    Number(1.5),
    BigInt(7),
    Str("a"),
    Bool(true),
    CodeUnit('x'),
    Enum("empty"),
    Undef,
    Null,
  )

  private val valueTys: List[ValueTy] = List(
    AnyT,
    BotT,
    NumberT,
    MathT,
    StrT,
    BoolT,
    UndefT,
    NullT,
    BigIntT,
    CodeUnitT,
    EnumT("empty"),
    ListT,
    NilT,
    MapT,
    RecordT("CompletionRecord"),
    AstT,
    AstT("A"),
    CloT,
    ContT,
    GrammarSymbolT,
    InfinityT,
    // unions across components
    NumberT || StrT,
    ListT(MathT) || RecordT("Symbol"),
  )

  // registration
  def init: Unit = {
    checkLaws("list lattice laws")(
      Domain[ListTy, Addr](
        listTys,
        listAddrs,
        (t, a) => t.contains(listObj(a), heap),
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.normalized,
        _.isBottom,
      ),
    )

    checkLaws("map lattice laws")(
      Domain[MapTy, Addr](
        mapTys,
        mapAddrs,
        (t, a) => t.contains(mapObj(a), heap),
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.normalized,
        _.isBottom,
      ),
    )

    checkLaws("ast lattice laws")(
      Domain[AstTy, AstValue](
        astTys,
        asts,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    checkLaws("bool lattice laws")(
      Domain[BoolTy, Boolean](
        boolTys,
        bools,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    checkLaws("bounded set lattice laws")(
      Domain[BSet[String], String](
        strTys,
        strs,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    checkLaws("record lattice laws")(
      Domain[RecordTy, Addr](
        recordTys,
        recordAddrs,
        (t, a) => t.contains(recordObj(a), heap),
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        _.normalized,
        _.isBottom,
      ),
    )

    checkLaws("value lattice laws")(
      Domain[ValueTy, Value](
        valueTys,
        values,
        (t, v) => t.contains(v, heap),
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )

    checkEqual("every component of `Any` is top")(
      AnyT.clo.isTop -> true,
      AnyT.cont.isTop -> true,
      AnyT.record.isTop -> true,
      AnyT.map.isTop -> true,
      AnyT.list.isTop -> true,
      AnyT.ast.isTop -> true,
      AnyT.grammarSymbol.isTop -> true,
      AnyT.codeUnit -> true,
      AnyT.enumv.isTop -> true,
      AnyT.math.isTop -> true,
      AnyT.infinity.isTop -> true,
      AnyT.number.isTop -> true,
      AnyT.bigInt -> true,
      AnyT.str.isTop -> true,
      AnyT.bool.isTop -> true,
      AnyT.undef -> true,
      AnyT.nullv -> true,
    )

    checkLaws("closure lattice laws")(
      Domain[CloTy, String](
        cloTys,
        cloNames,
        _ contains _,
        _ <= _,
        _ -- _,
        _ || _,
        _ && _,
        identity,
        _.isBottom,
      ),
    )
  }

  init
}
