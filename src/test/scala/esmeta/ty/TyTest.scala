package esmeta.ty

import esmeta.cfg.*
import esmeta.es.*
import esmeta.interpreter.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.state.{BigInt as StateBigInt, *}
import esmeta.util.*
import esmeta.ESMetaTest
import scala.collection.mutable.{Map => MMap, LinkedHashMap => LMMap}

/** test for types, holding the fixtures, the type universes, and the law
  * harness that every `ty` test shares
  */
trait TyTest extends ESMetaTest {
  def category: String = "ty"

  // ---------------------------------------------------------------------------
  // type declarations and field maps
  // ---------------------------------------------------------------------------
  // type declaration elements
  import TyDecl.Elem.*
  val absMethod = AbsMethod("a")
  val conMethod = ConMethod("b", false, None)
  val conMethodOpt = ConMethod("c", true, None)
  val conMethodTarget = ConMethod("d", false, Some("foo"))
  val conMethodOptTarget = ConMethod("e", true, Some("bar"))

  // type declarations
  val decl0 = TyDecl("A", None, Nil)
  val decl1 = TyDecl("A", None, List(absMethod))
  val decl2 = TyDecl("A", None, List(absMethod, conMethodOpt))
  val declParent0 = TyDecl("A", Some("B", true), Nil)
  val declParent1 = TyDecl("A", Some("B", false), List(absMethod))
  val declParent2 = TyDecl("A", Some("B", true), List(absMethod, conMethodOpt))

  // type models
  val tyModel0 = TyModel(Nil)
  val tyModel1 = TyModel(List(decl0))
  val tyModel2 = TyModel(List(declParent0, decl1))
  val tyModel3 = TyModel(List(decl0, declParent1, decl2))

  // field type map
  val fieldMap0 = FieldMap()
  val fieldMap1 = FieldMap("p" -> Binding(AnyT, false))
  val fieldMap2 = FieldMap(
    "p" -> Binding(AnyT, false),
    "q" -> Binding(BoolT, false),
    "r" -> Binding(UndefT, true),
  )

  // ---------------------------------------------------------------------------
  // numeric witnesses and universes
  // ---------------------------------------------------------------------------
  protected def numSet(ds: Double*) = NumberTy(ds.map(Number(_)).toSet)
  protected def mathSet(ds: BigDecimal*) = MathSetTy(ds.map(Math(_)).toSet)
  protected def intSet(xs: BigInt*) = IntSetTy(xs.toSet)

  protected val huge = BigInt("100000000000000000000")

  /** concrete values used to witness the abstract operations */
  protected val numbers = List(
    Double.NegativeInfinity,
    -3.0,
    -2.5,
    -1.0,
    -0.0,
    0.0,
    1.0,
    2.5,
    3.0,
    3.0e9, // beyond Int range
    9007199254740992.0, // 2^53, the last exactly represented integer
    Double.MinPositiveValue, // a product that underflows to a zero
    Double.MaxValue, // a sum that overflows to an infinity
    Double.PositiveInfinity,
    Double.NaN,
  ).map(Number(_))
  protected val maths = List[BigDecimal](
    -3,
    -2.5,
    -1,
    -0.5,
    0,
    0.5,
    1,
    2.5,
    3,
    BigDecimal(huge),
  ).map(Math(_))
  protected val bigInts = List[BigInt](-3, -1, 0, 1, 3, huge)

  /** every shape of every domain, including the redundant encodings of top and
    * bottom that `canon` is meant to collapse
    */
  protected val numberTys: List[NumberTy] = List(
    NumberTy.Top,
    NumberTy.Bot,
    NumberTy.NaN,
    NumberTy.Infinite,
    NumberTy.Pos,
    NumberTy.Neg,
    NumberTy.NonNeg,
    NumberTy.NonPos,
    NumberTy.NonZero,
    NumberTy.Int,
    NumberTy.PosInt,
    NumberTy.NegInt,
    NumberTy.NonNegInt,
    NumberTy.NonPosInt,
    NumberTy.Zero,
    NumberTy.One,
    // redundant encodings
    NumberTy.Finite,
    NumberTy(FinNumberSignTy(Sign.Pos), InfinityTy.Bot, true),
    NumberTy.Bot,
    NumberTy.NaN,
    NumberTy.finite(FinNumberSignTy(Sign.Zero)),
    NumberTy(FinNumberIntTy(IntTy.Top), InfinityTy.Bot, true),
    NumberTy.NaN,
    NumberTy.int(IntSignTy(Sign.Bot)),
    numSet(),
    // a finite part alongside each special value
    NumberTy(FinNumberIntTy(IntTy.Top), InfinityTy.Top, false),
    NumberTy(FinNumberSignTy(Sign.Pos), InfinityTy.Neg, false),
    NumberTy(FinNumberSignTy(Sign.NonNeg), InfinityTy.Top, true),
    // signed zeros, NaN, and infinities
    numSet(0.0),
    numSet(-0.0),
    numSet(0.0, -0.0),
    numSet(Double.NaN),
    NumberTy.PosInf,
    NumberTy.NegInf,
    numSet(Double.PositiveInfinity, Double.NegativeInfinity, Double.NaN),
    numSet(Double.PositiveInfinity, 0.0, -0.0),
    // non-integral and out-of-Int-range values
    numSet(2.5),
    // the boundary of exact integer representation, and beyond it
    NumberTy.int(intSet(BigInt(2).pow(53))),
    numSet(Double.MaxValue),
    numSet(1.0, 2.5),
    numSet(Double.NaN, 2.5),
    numSet(3.0e9),
    NumberTy.int(intSet(3000000000L)),
    NumberTy.int(intSet(huge)),
  )
  protected val mathTys: List[MathTy] = List(
    MathTy.Top,
    MathTy.Bot,
    MathTy.Pos,
    MathTy.Neg,
    MathTy.NonNeg,
    MathTy.NonPos,
    MathTy.Int,
    MathTy.PosInt,
    MathTy.NegInt,
    MathTy.NonNegInt,
    MathTy.NonPosInt,
    MathTy.Zero,
    MathTy.One,
    // redundant encodings
    MathSignTy(Sign.Bot),
    MathSignTy(Sign.Zero),
    MathIntTy(IntTy.Bot),
    MathIntTy(IntSignTy(Sign.Bot)),
    mathSet(),
    // sets mixing integral and non-integral values
    mathSet(0),
    mathSet(1),
    mathSet(0.5),
    mathSet(-0.5),
    mathSet(-0.5, 0.5),
    mathSet(1, 0.5),
    mathSet(-1, 0, 1),
    mathSet(BigDecimal(huge)),
    MathIntTy(intSet(-1, 1)),
    MathIntTy(intSet(huge)),
  )
  protected val intTys: List[IntTy] = List(
    IntTy.Top,
    IntTy.Bot,
    IntTy.Pos,
    IntTy.Neg,
    IntTy.NonNeg,
    IntTy.NonPos,
    IntTy.Zero,
    IntTy.One,
    // redundant encodings
    IntSignTy(Sign.Bot),
    IntSignTy(Sign.Zero),
    intSet(),
    intSet(-1, 1),
    intSet(0, 3),
    intSet(huge),
    intSet(-huge, huge),
  )

  // ---------------------------------------------------------------------------
  // non-numeric witnesses and universes
  // ---------------------------------------------------------------------------
  protected val F = false
  protected val T = true

  protected val func = Func(
    0,
    IRFunc(true, IRFuncKind.AbsOp, "f", Nil, UnknownType, INop()),
    Block(0),
  )

  // lists, including the empty one, which belongs to every element type
  protected val nilAddr = NamedAddr("nil")
  protected val mathListAddr = NamedAddr("mathList")
  protected val strListAddr = NamedAddr("strList")
  protected val mixedListAddr = NamedAddr("mixedList")

  // maps, including the empty one, which belongs to every key/value type
  protected val emptyMapAddr = NamedAddr("emptyMap")
  protected val numMapAddr = NamedAddr("numMap")
  protected val strMapAddr = NamedAddr("strMap")

  // records
  protected val recordAddr = NamedAddr("record")
  protected val symbolAddr = NamedAddr("symbol")
  protected val normalAddr = NamedAddr("normal")
  protected val abruptAddr = NamedAddr("abrupt")
  protected val completionAddr = NamedAddr("completion")

  protected val heap: Heap = Heap(
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

  protected def listObj(a: Addr): ListObj =
    heap(a).asInstanceOf[ListObj]
  protected def mapObj(a: Addr): MapObj =
    heap(a).asInstanceOf[MapObj]

  protected def recordObj(a: Addr): RecordObj =
    heap(a).asInstanceOf[RecordObj]

  protected val recordAddrs =
    List(recordAddr, symbolAddr, normalAddr, abruptAddr, completionAddr)

  protected val listAddrs =
    List(nilAddr, mathListAddr, strListAddr, mixedListAddr)
  protected val mapAddrs = List(emptyMapAddr, numMapAddr, strMapAddr)

  protected val astA0 = Syntactic("A", List(F, T), 0, Vector.empty)
  protected val astA1 = Syntactic("A", List(F, T), 1, Vector.empty)
  protected val astB5 = Syntactic("B", List(F, T), 5, Vector(Some(astA0), None))
  protected val asts = List(astA0, astA1, astB5).map(AstValue(_))

  protected val strs = List("a", "b", "c")
  protected val bools = List(true, false)

  // ---------------------------------------------------------------------------
  // type universes, including the redundant encodings of top and bottom
  // ---------------------------------------------------------------------------
  protected val listTys: List[ListTy] = List(
    ListTy.Top,
    ListTy.Bot,
    ListTy.Nil,
    ListTy.Elem(MathT),
    ListTy.Elem(StrT),
    ListTy.Elem(MathT || StrT),
    // redundant encoding
    ListTy.Elem(AnyT),
  )

  protected val mapTys: List[MapTy] = List(
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

  protected val astTys: List[AstTy] = List(
    AstTy.Top,
    AstTy.Bot,
    AstTy.Simple(Set("A")),
    AstTy.Simple(Set("B")),
    AstTy.Simple(Set("A", "B")),
    AstTy.Detail("A", 0),
    AstTy.Detail("A", 1),
    AstTy.Detail("B", 5),
  )

  protected val boolTys: List[BoolTy] = List(
    BoolTy.Top,
    BoolTy.Bot,
    BoolTy(true),
    BoolTy(false),
  )

  protected val strTys: List[BSet[String]] = List(
    Inf,
    Fin[String](),
    Fin("a"),
    Fin("b"),
    Fin("a", "b"),
  )

  protected val cloTys: List[CloTy] = List(
    CloTy.Top,
    CloTy.Bot,
    CloSetTy(Set("f")),
    CloSetTy(Set("g")),
    CloSetTy(Set("f", "g")),
  )

  protected val cloNames = List("f", "g", "h")

  /** real declarations from the type model, including a parent and two of its
    * children, so that subtyping through `TyModel` is exercised
    */
  protected val recordTys: List[RecordTy] = List(
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
  protected val valueWitnesses: List[Value] = List(
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
    StateBigInt(7),
    Str("a"),
    Bool(true),
    CodeUnit('x'),
    Enum("empty"),
    Undef,
    Null,
  )

  protected val valueTys: List[ValueTy] = List(
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

  // ---------------------------------------------------------------------------
  // the law harness
  // ---------------------------------------------------------------------------
  /** a domain to check the lattice laws of */
  protected case class Domain[T, V](
    tys: List[T],
    values: List[V],
    contains: (T, V) => Boolean,
    le: (T, T) => Boolean,
    prune: (T, T) => T,
    join: (T, T) => T,
    meet: (T, T) => T,
    canon: T => T,
    isBottom: T => Boolean,
  )

  /** Check that each abstract operation over-approximates its concrete
    * counterpart: no value that belongs in the result may be missing from it. A
    * violation means the analyzer may drop a reachable value, which is exactly
    * the class of bug these operations have had.
    */
  protected def checkLaws[T, V](desc: String)(d: Domain[T, V]): Unit =
    import d.*
    def has(t: T, v: V) = contains(t, v)
    val violations = (for {
      l <- tys
      // canon must not change the meaning of a type
      v <- values
      if has(l, v) != has(canon(l), v)
    } yield s"canon changes membership of $v in $l") ++ (for {
      l <- tys
      r <- tys
      v <- values
      (op, ty, expected) <- List(
        ("--", prune(l, r), has(l, v) && !has(r, v)),
        ("||", join(l, r), has(l, v) || has(r, v)),
        ("&&", meet(l, r), has(l, v) && has(r, v)),
      )
      if expected && !has(ty, v)
    } yield s"$v is missing from ($l $op $r) = $ty") ++ (for {
      l <- tys
      r <- tys
      // the order must agree with containment, and bound each result
      msg <-
        (if (le(l, r) && values.exists(v => has(l, v) && !has(r, v)))
           List(s"$l <= $r but they differ on a concrete value")
         else Nil) ++
        (if (!le(l, join(l, r))) List(s"$l is not below ($l || $r)")
         else Nil) ++
        (if (!le(meet(l, r), l)) List(s"($l && $r) is not below $l")
         else Nil) ++
        (if (!le(prune(l, r), l)) List(s"($l -- $r) is not below $l") else Nil)
    } yield msg) ++ (for {
      l <- tys
      if !isBottom(prune(l, l))
    } yield s"($l -- $l) is not bottom")
    check(desc) {
      if (violations.nonEmpty) {
        println(s"[FAILED] $desc: ${violations.size} violation(s)")
        violations.distinct.take(10).foreach(v => println(s"- $v"))
        assert(violations.isEmpty)
      }
    }
}
