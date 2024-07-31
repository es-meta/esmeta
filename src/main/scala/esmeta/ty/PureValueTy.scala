package esmeta.ty

import esmeta.cfg.Func
import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** pure value types (non-completion record types) */
sealed trait PureValueTy extends TyElem with Lattice[PureValueTy] {
  import PureValueTy.*

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
  def <=(that: => PureValueTy): Boolean =
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
  def ||(that: => PureValueTy): PureValueTy =
    if (this eq that) this
    else if (this eq Bot) that
    else if (this eq Top) Top
    else if (that eq Bot) this
    else if (that eq Top) Top
    else
      PureValueTy(
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
  def &&(that: => PureValueTy): PureValueTy =
    if (this eq that) this
    else if (this eq Bot) Bot
    else if (this eq Top) that
    else if (that eq Bot) Bot
    else if (that eq Top) this
    else
      PureValueTy(
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
  def --(that: => PureValueTy): PureValueTy =
    if (this eq that) Bot
    else if (this eq Bot) Bot
    else if (that eq Bot) this
    else if (that eq Top) Bot
    else
      PureValueTy(
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

  /** normalization */
  def norm: PureValueTy = if (
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
  ) PureValueTy.Top
  else this

  /** get single value */
  def getSingle: Flat[PureValue] =
    (if (this.clo.isBottom) Zero else Many) ||
    (if (this.cont.isBottom) Zero else Many) ||
    (if (this.record.isBottom) Zero else Many) ||
    (if (this.map.isBottom) Zero else Many) ||
    (if (this.list.isBottom) Zero else Many) ||
    (if (this.ast.isBottom) Zero else Many) ||
    grammarSymbol.getSingle ||
    (if (this.codeUnit.isBottom) Zero else Many) ||
    (enumv.getSingle.map(Enum(_): PureValue)) ||
    math.getSingle ||
    (infinity.getSingle.map(Infinity(_): PureValue)) ||
    number.getSingle ||
    (if (this.bigInt.isBottom) Zero else Many) ||
    (str.getSingle.map(Str(_): PureValue)) ||
    (bool.getSingle.map(Bool(_): PureValue)) ||
    (if (this.undef.isBottom) Zero else One(Undef)) ||
    (if (this.nullv.isBottom) Zero else One(Null))

  /** types having no field */
  def noField: PureValueTy = this match
    case PureValueTopTy =>
      PureValueElemTy(
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
    case elem: PureValueElemTy =>
      elem.copy(
        record = RecordTy.Bot,
        map = MapTy.Bot,
        list = ListTy.Bot,
        ast = AstTy.Bot,
        str = Fin(),
      )
}

case object PureValueTopTy extends PureValueTy {
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

case class PureValueElemTy(
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
) extends PureValueTy
object PureValueTy extends Parser.From(Parser.pureValueTy) {
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
  ): PureValueTy = PureValueElemTy(
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
  lazy val Top: PureValueTy = PureValueTopTy
  lazy val Bot: PureValueTy = PureValueElemTy()
}
