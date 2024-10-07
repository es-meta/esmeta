package esmeta.ty

import esmeta.cfg.Func
import esmeta.error.*
import esmeta.error.NotSupported.Category.*
import esmeta.error.NotSupported.{*, given}
import esmeta.peval.{Known, Unknown, Predict}
import esmeta.peval.pstate.{PHeap}
import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.peval.Unknown

/** value types */
sealed trait ValueTy extends Ty with Lattice[ValueTy] {
  import ValueTy.*

  def clo: BSet[String]
  def cont: BSet[Int]
  def record: RecordTy
  def map: MapTy
  def list: ListTy
  def ast: AstTy
  def grammarSymbol: BSet[GrammarSymbol]
  def codeUnit: Boolean
  def enumv: BSet[String]
  def math: MathTy
  def infinity: InfinityTy
  def number: NumberTy
  def bigInt: Boolean
  def str: BSet[String]
  def bool: BoolTy
  def undef: Boolean
  def nullv: Boolean

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean =
    if (this eq Bot) true
    else if (this eq Top) false
    else
      (
        this.clo.isBottom &&
        this.cont.isBottom &&
        this.record.isBottom &&
        this.map.isBottom &&
        this.list.isBottom &&
        this.ast.isBottom &&
        this.grammarSymbol.isBottom &&
        this.codeUnit.isBottom &&
        this.enumv.isBottom &&
        this.math.isBottom &&
        this.infinity.isBottom &&
        this.number.isBottom &&
        this.bigInt.isBottom &&
        this.str.isBottom &&
        this.bool.isBottom &&
        this.undef.isBottom &&
        this.nullv.isBottom
      )

  /** partial order/subset operator */
  def <=(that: => ValueTy): Boolean =
    if ((this eq that) || (this eq Bot) || (that eq Top)) true
    else if (this eq Top) false
    else
      this.clo <= that.clo &&
      this.cont <= that.cont &&
      this.record <= that.record &&
      this.map <= that.map &&
      this.list <= that.list &&
      this.ast <= that.ast &&
      this.grammarSymbol <= that.grammarSymbol &&
      this.codeUnit <= that.codeUnit &&
      this.enumv <= that.enumv &&
      this.math <= that.math &&
      this.infinity <= that.infinity &&
      this.number <= that.number &&
      this.bigInt <= that.bigInt &&
      this.str <= that.str &&
      this.bool <= that.bool &&
      this.undef <= that.undef &&
      this.nullv <= that.nullv

  /** union type */
  def ||(that: => ValueTy): ValueTy =
    if (this eq that) this
    else if (this eq Bot) that
    else if (this eq Top) Top
    else if (that eq Bot) this
    else if (that eq Top) Top
    else
      ValueTy(
        this.clo || that.clo,
        this.cont || that.cont,
        this.record || that.record,
        this.map || that.map,
        this.list || that.list,
        this.ast || that.ast,
        this.grammarSymbol || that.grammarSymbol,
        this.codeUnit || that.codeUnit,
        this.enumv || that.enumv,
        this.math || that.math,
        this.infinity || that.infinity,
        this.number || that.number,
        this.bigInt || that.bigInt,
        this.str || that.str,
        this.bool || that.bool,
        this.undef || that.undef,
        this.nullv || that.nullv,
      ).norm

  /** intersection type */
  def &&(that: => ValueTy): ValueTy =
    if (this eq that) this
    else if (this eq Bot) Bot
    else if (this eq Top) that
    else if (that eq Bot) Bot
    else if (that eq Top) this
    else
      ValueTy(
        this.clo && that.clo,
        this.cont && that.cont,
        this.record && that.record,
        this.map && that.map,
        this.list && that.list,
        this.ast && that.ast,
        this.grammarSymbol && that.grammarSymbol,
        this.codeUnit && that.codeUnit,
        this.enumv && that.enumv,
        this.math && that.math,
        this.infinity && that.infinity,
        this.number && that.number,
        this.bigInt && that.bigInt,
        this.str && that.str,
        this.bool && that.bool,
        this.undef && that.undef,
        this.nullv && that.nullv,
      )

  /** prune type */
  def --(that: => ValueTy): ValueTy =
    if (this eq that) Bot
    else if (this eq Bot) Bot
    else if (that eq Bot) this
    else if (that eq Top) Bot
    else
      ValueTy(
        this.clo -- that.clo,
        this.cont -- that.cont,
        this.record -- that.record,
        this.map -- that.map,
        this.list -- that.list,
        this.ast -- that.ast,
        this.grammarSymbol -- that.grammarSymbol,
        this.codeUnit -- that.codeUnit,
        this.enumv -- that.enumv,
        this.math -- that.math,
        this.infinity -- that.infinity,
        this.number -- that.number,
        this.bigInt -- that.bigInt,
        this.str -- that.str,
        this.bool -- that.bool,
        this.undef -- that.undef,
        this.nullv -- that.nullv,
      )

  /** value containment check */
  def contains(value: Value, heap: Heap): Boolean = value match
    case _ if this eq Top => true
    case a: Addr =>
      heap(a) match
        case obj: RecordObj => record.contains(obj, heap)
        case obj: MapObj    => map.contains(obj, heap)
        case obj: ListObj   => list.contains(obj, heap)
        case obj: YetObj    => throw NotSupported(Feature)(obj.msg)
    case Clo(func, captured)             => clo contains func.irFunc.name
    case Cont(func, captured, callStack) => cont contains func.id
    case v: AstValue                     => ast.contains(v)
    case x @ GrammarSymbol(name, params) => grammarSymbol contains x
    case m: Math                         => math contains m
    case Infinity(p)                     => infinity contains p
    case Enum(name)                      => enumv contains name
    case CodeUnit(c)                     => codeUnit
    case n: Number                       => number contains n
    case BigInt(n)                       => bigInt
    case Str(s)                          => str contains s
    case Bool(b)                         => bool contains b
    case Undef                           => undef
    case Null                            => nullv
    case PClo(func, _)                   => clo contains func.irFunc.name
    case PCont(func, _, _)               => cont contains func.id

  def contains(pv: Predict[Value], pheap: PHeap): Predict[Boolean] =
    pv.flatMap {
      _ match
        case _ if this eq Top => Known(true)
        case a: Addr =>
          import esmeta.peval.pstate.*;
          pheap(a) match
            case Unknown => ??? // TODO
            case Known(obj: PRecordObj) =>
              ??? // TODO: record.contains(obj, pheap)
            case Known(obj: PMapObj)  => ??? // TODO: map.contains(obj, pheap)
            case Known(obj: PListObj) => ??? // TODO: list.contains(obj, pheap)
            case Known(obj: PYetObj)  => throw NotSupported(Feature)(obj.msg)
        case Clo(func, captured) => Known(clo contains func.irFunc.name)
        case Cont(func, captured, callStack) => Known(cont contains func.id)
        case v: AstValue                     => Known(ast.contains(v))
        case x @ GrammarSymbol(name, params) => Known(grammarSymbol contains x)
        case m: Math                         => Known(math contains m)
        case Infinity(p)                     => Known(infinity contains p)
        case Enum(name)                      => Known(enumv contains name)
        case CodeUnit(c)                     => Known(codeUnit)
        case n: Number                       => Known(number contains n)
        case BigInt(n)                       => Known(bigInt)
        case Str(s)                          => Known(str contains s)
        case Bool(b)                         => Known(bool contains b)
        case Undef                           => Known(undef)
        case Null                            => Known(nullv)
        case PClo(func, _)     => Known(clo contains func.irFunc.name)
        case PCont(func, _, _) => Known(cont contains func.id)
    }

  /** copy value type */
  def copied(
    clo: BSet[String] = clo,
    cont: BSet[Int] = cont,
    record: RecordTy = record,
    map: MapTy = map,
    list: ListTy = list,
    ast: AstTy = ast,
    grammarSymbol: BSet[GrammarSymbol] = grammarSymbol,
    codeUnit: Boolean = codeUnit,
    enumv: BSet[String] = enumv,
    math: MathTy = math,
    infinity: InfinityTy = infinity,
    number: NumberTy = number,
    bigInt: Boolean = bigInt,
    str: BSet[String] = str,
    bool: BoolTy = bool,
    undef: Boolean = undef,
    nullv: Boolean = nullv,
  ): ValueTy = ValueTy(
    clo,
    cont,
    record,
    map,
    list,
    ast,
    grammarSymbol,
    codeUnit,
    enumv,
    math,
    infinity,
    number,
    bigInt,
    str,
    bool,
    undef,
    nullv,
  )

  /** completion check */
  def isCompletion: Boolean = this <= CompT

  /** normalization */
  def norm: ValueTy = if (
    clo.isTop &&
    cont.isTop &&
    record.isTop &&
    map.isTop &&
    list.isTop &&
    ast.isTop &&
    grammarSymbol.isTop &&
    codeUnit.isTop &&
    enumv.isTop &&
    math.isTop &&
    infinity.isTop &&
    number.isTop &&
    bigInt.isTop &&
    str.isTop &&
    bool.isTop &&
    undef.isTop &&
    nullv.isTop
  ) ValueTy.Top
  else this

  /** get single value */
  def getSingle: Flat[Value] =
    (if (this.clo.isBottom) Zero else Many) ||
    (if (this.cont.isBottom) Zero else Many) ||
    (if (this.record.isBottom) Zero else Many) ||
    (if (this.map.isBottom) Zero else Many) ||
    (if (this.list.isBottom) Zero else Many) ||
    (if (this.ast.isBottom) Zero else Many) ||
    grammarSymbol.getSingle ||
    (if (this.codeUnit.isBottom) Zero else Many) ||
    (enumv.getSingle.map(Enum(_): Value)) ||
    math.getSingle ||
    (infinity.getSingle.map(Infinity(_): Value)) ||
    number.getSingle ||
    (if (this.bigInt.isBottom) Zero else Many) ||
    (str.getSingle.map(Str(_): Value)) ||
    (bool.getSingle.map(Bool(_): Value)) ||
    (if (this.undef.isBottom) Zero else One(Undef)) ||
    (if (this.nullv.isBottom) Zero else One(Null))

  /** types having no field */
  def noField: ValueTy = this match
    case ValueTopTy =>
      ValueElemTy(
        clo = Inf,
        cont = Inf,
        grammarSymbol = Inf,
        codeUnit = true,
        enumv = Inf,
        math = MathTy.Top,
        number = NumberTy.Top,
        bigInt = true,
        bool = BoolTy.Top,
        undef = true,
        nullv = true,
      )
    case elem: ValueElemTy =>
      elem.copy(
        record = RecordTy.Bot,
        map = MapTy.Bot,
        list = ListTy.Bot,
        ast = AstTy.Bot,
        str = Fin(),
      )
}

case object ValueTopTy extends ValueTy {
  def clo: BSet[String] = Inf
  def cont: BSet[Int] = Inf
  def record: RecordTy = RecordTy.Top
  def map: MapTy = MapTy.Top
  def list: ListTy = ListTy.Bot // unsound but need to remove cycle
  def ast: AstTy = AstTy.Top
  def grammarSymbol: BSet[GrammarSymbol] = Inf
  def codeUnit: Boolean = true
  def enumv: BSet[String] = Inf
  def math: MathTy = MathTy.Top
  def infinity: InfinityTy = InfinityTy.Top
  def number: NumberTy = NumberTy.Top
  def bigInt: Boolean = true
  def str: BSet[String] = Inf
  def bool: BoolTy = BoolTy.Top
  def undef: Boolean = true
  def nullv: Boolean = true
}

case class ValueElemTy(
  clo: BSet[String] = Fin(),
  cont: BSet[Int] = Fin(),
  record: RecordTy = RecordTy.Bot,
  map: MapTy = MapTy.Bot,
  list: ListTy = ListTy.Bot,
  ast: AstTy = AstTy.Bot,
  grammarSymbol: BSet[GrammarSymbol] = Fin(),
  codeUnit: Boolean = false,
  enumv: BSet[String] = Fin(),
  math: MathTy = MathTy.Bot,
  infinity: InfinityTy = InfinityTy.Bot,
  number: NumberTy = NumberTy.Bot,
  bigInt: Boolean = false,
  str: BSet[String] = Fin(),
  bool: BoolTy = BoolTy.Bot,
  undef: Boolean = false,
  nullv: Boolean = false,
) extends ValueTy
object ValueTy extends Parser.From(Parser.valueTy) {
  def apply(
    clo: BSet[String] = Fin(),
    cont: BSet[Int] = Fin(),
    record: RecordTy = RecordTy.Bot,
    map: MapTy = MapTy.Bot,
    list: ListTy = ListTy.Bot,
    ast: AstTy = AstTy.Bot,
    grammarSymbol: BSet[GrammarSymbol] = Fin(),
    codeUnit: Boolean = false,
    enumv: BSet[String] = Fin(),
    math: MathTy = MathTy.Bot,
    infinity: InfinityTy = InfinityTy.Bot,
    number: NumberTy = NumberTy.Bot,
    bigInt: Boolean = false,
    str: BSet[String] = Fin(),
    bool: BoolTy = BoolTy.Bot,
    undef: Boolean = false,
    nullv: Boolean = false,
  ): ValueTy = ValueElemTy(
    clo,
    cont,
    record,
    map,
    list,
    ast,
    grammarSymbol,
    codeUnit,
    enumv,
    math,
    infinity,
    number,
    bigInt,
    str,
    bool,
    undef,
    nullv,
  ).norm
  lazy val Top: ValueTy = ValueTopTy
  lazy val Bot: ValueTy = ValueElemTy()

  /** get type from `typeof` string */
  def fromTypeOf(str: String): ValueTy = str match
    case "Object"    => RecordT("Object")
    case "Symbol"    => SymbolT
    case "Number"    => NumberT
    case "BigInt"    => BigIntT
    case "String"    => StrT
    case "Boolean"   => BoolT
    case "Undefined" => UndefT
    case "Null"      => NullT
    case _           => Bot
}
