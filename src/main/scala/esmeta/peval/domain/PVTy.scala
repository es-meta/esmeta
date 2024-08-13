package esmeta.peval.domain

import esmeta.cfg.Func
import esmeta.error.*
import esmeta.error.NotSupported.Category.*
import esmeta.error.NotSupported.{*, given}
import esmeta.state.*
import esmeta.ty.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** partial value types */
sealed trait PVTy extends Ty with Lattice[PVTy] {
  import PVTy.*

  def addr: BSet[Addr]
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
        this.addr.isBottom &&
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
  def <=(that: => PVTy): Boolean =
    if ((this eq that) || (this eq Bot) || (that eq Top)) true
    else if (this eq Top) false
    else
      this.addr <= that.addr &&
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
  def ||(that: => PVTy): PVTy =
    if (this eq that) this
    else if (this eq Bot) that
    else if (this eq Top) Top
    else if (that eq Bot) this
    else if (that eq Top) Top
    else
      PVTy(
        this.addr || this.addr,
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
  def &&(that: => PVTy): PVTy =
    if (this eq that) this
    else if (this eq Bot) Bot
    else if (this eq Top) that
    else if (that eq Bot) Bot
    else if (that eq Top) this
    else
      PVTy(
        this.addr && that.addr,
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
  def --(that: => PVTy): PVTy =
    if (this eq that) Bot
    else if (this eq Bot) Bot
    else if (that eq Bot) this
    else if (that eq Top) Bot
    else
      PVTy(
        this.addr -- that.addr,
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
    case a: Addr                         => addr contains a
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
    case _                               => ???

  /** copy value type */
  def copied(
    addr: BSet[Addr] = addr,
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
  ): PVTy = PVTy(
    addr,
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
  def isCompletion: Boolean = ??? // this <= CompT

  /** normalization */
  def norm: PVTy = if (
    addr.isTop &&
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
  ) PVTy.Top
  else this

  /** get single value */
  def getSingle: Flat[Value] =
    (addr.getSingle.map((a) => a: Value)) ||
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
  def noField: PVTy = this match
    case PVTopTy =>
      PVElemTy(
        addr = Inf,
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
    case elem: PVElemTy =>
      elem.copy(
        addr = Fin(),
        str = Fin(),
      )
}

case object PVTopTy extends PVTy {
  def addr: BSet[Addr] = Inf
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

case class PVElemTy(
  addr: BSet[Addr] = Fin(),
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
) extends PVTy
object PVTy { // extends Parser.From(Parser.PVTy)
  def apply(
    addr: BSet[Addr] = Fin(),
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
  ): PVTy = PVElemTy(
    addr,
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
  lazy val Top: PVTy = PVTopTy
  lazy val Bot: PVTy = PVElemTy()
  lazy val StrT = PVElemTy(str = Inf)
  lazy val NullT = PVElemTy(nullv = true)
  lazy val UndefT = PVElemTy(undef = true)
  lazy val BoolT = PVElemTy(bool = BoolTy.Top)
  lazy val TrueT = PVElemTy(bool = BoolTy(Set(true)))
  lazy val FalseT = PVElemTy(bool = BoolTy(Set(false)))
  lazy val NumberT = PVElemTy(number = NumberTopTy)
  lazy val BigIntT = PVElemTy(bigInt = true)

}
