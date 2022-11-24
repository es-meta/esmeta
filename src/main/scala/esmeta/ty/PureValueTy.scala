package esmeta.ty

import esmeta.cfg.Func
import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.analyzer.domain.*

/** pure value types (non-completion record types) */
sealed trait PureValueTy extends TyElem with Lattice[PureValueTy] {
  import PureValueTy.*

  def clo: BSet[String]
  def cont: BSet[Int]
  def name: NameTy
  def record: RecordTy
  def list: ListTy
  def symbol: Boolean
  def astValue: AstValueTy
  def nt: BSet[Nt]
  def codeUnit: Boolean
  def const: BSet[String]
  def math: BSet[BigDecimal]
  def number: BSet[Number]
  def bigInt: Boolean
  def str: BSet[String]
  def bool: BoolTy
  def undef: Boolean
  def nullv: Boolean
  def absent: Boolean

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
        this.name.isBottom &&
        this.record.isBottom &&
        this.list.isBottom &&
        this.symbol.isBottom &&
        this.astValue.isBottom &&
        this.nt.isBottom &&
        this.codeUnit.isBottom &&
        this.const.isBottom &&
        this.math.isBottom &&
        this.number.isBottom &&
        this.bigInt.isBottom &&
        this.str.isBottom &&
        this.bool.isBottom &&
        this.undef.isBottom &&
        this.nullv.isBottom &&
        this.absent.isBottom
      )

  /** partial order/subset operator */
  def <=(that: => PureValueTy): Boolean =
    if ((this eq that) || (this eq Bot) || (that eq Top)) true
    else if (this eq Top) false
    else
      this.clo <= that.clo &&
      this.cont <= that.cont &&
      this.name <= that.name &&
      this.record <= that.record &&
      this.list <= that.list &&
      this.symbol <= that.symbol &&
      this.astValue <= that.astValue &&
      this.nt <= that.nt &&
      this.codeUnit <= that.codeUnit &&
      this.const <= that.const &&
      this.math <= that.math &&
      this.number <= that.number &&
      this.bigInt <= that.bigInt &&
      this.str <= that.str &&
      this.bool <= that.bool &&
      this.undef <= that.undef &&
      this.nullv <= that.nullv &&
      this.absent <= that.absent

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
        this.name || that.name,
        this.record || that.record,
        this.list || that.list,
        this.symbol || that.symbol,
        this.astValue || that.astValue,
        this.nt || that.nt,
        this.codeUnit || that.codeUnit,
        this.const || that.const,
        this.math || that.math,
        this.number || that.number,
        this.bigInt || that.bigInt,
        this.str || that.str,
        this.bool || that.bool,
        this.undef || that.undef,
        this.nullv || that.nullv,
        this.absent || that.absent,
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
        this.name && that.name,
        this.record && that.record,
        this.list && that.list,
        this.symbol && that.symbol,
        this.astValue && that.astValue,
        this.nt && that.nt,
        this.codeUnit && that.codeUnit,
        this.const && that.const,
        this.math && that.math,
        this.number && that.number,
        this.bigInt && that.bigInt,
        this.str && that.str,
        this.bool && that.bool,
        this.undef && that.undef,
        this.nullv && that.nullv,
        this.absent && that.absent,
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
        this.name -- that.name,
        this.record -- that.record,
        this.list -- that.list,
        this.symbol -- that.symbol,
        this.astValue -- that.astValue,
        this.nt -- that.nt,
        this.codeUnit -- that.codeUnit,
        this.const -- that.const,
        this.math -- that.math,
        this.number -- that.number,
        this.bigInt -- that.bigInt,
        this.str -- that.str,
        this.bool -- that.bool,
        this.undef -- that.undef,
        this.nullv -- that.nullv,
        this.absent -- that.absent,
      )

  /** get single value */
  def getSingle: Flat[APureValue] =
    (if (this.clo.isBottom) Zero else Many) ||
    (if (this.cont.isBottom) Zero else Many) ||
    (if (this.name.isBottom) Zero else Many) ||
    (if (this.record.isBottom) Zero else Many) ||
    (if (this.list.isBottom) Zero else Many) ||
    (if (this.symbol.isBottom) Zero else Many) ||
    (if (this.astValue.isBottom) Zero else Many) ||
    (if (this.nt.isBottom) Zero else Many) ||
    (if (this.codeUnit.isBottom) Zero else Many) ||
    (if (this.const.isBottom) Zero else Many) ||
    (if (this.math.isBottom) Zero else Many) ||
    (if (this.number.isBottom) Zero else Many) ||
    (if (this.bigInt.isBottom) Zero else Many) ||
    (str.getSingle.map(Str(_))) ||
    (bool.getSingle.map(Bool(_))) ||
    (if (this.undef.isBottom) Zero else One(Undef)) ||
    (if (this.nullv.isBottom) Zero else One(Null)) ||
    (if (this.absent.isBottom) Zero else One(Absent))

  /** normalization */
  def norm: PureValueTy = if (
    clo.isTop &&
    cont.isTop &&
    name.isTop &&
    record.isTop &&
    list.isTop &&
    symbol.isTop &&
    astValue.isTop &&
    nt.isTop &&
    codeUnit.isTop &&
    const.isTop &&
    math.isTop &&
    number.isTop &&
    bigInt.isTop &&
    str.isTop &&
    bool.isTop &&
    undef.isTop &&
    nullv.isTop &&
    absent.isTop
  ) PureValueTy.Top
  else this
}

case object PureValueTopTy extends PureValueTy {
  def clo: BSet[String] = Inf
  def cont: BSet[Int] = Inf
  def name: NameTy = NameTy.Top
  def record: RecordTy = RecordTy.Top
  def list: ListTy = ListTy.Bot // unsound but need to remove cycle
  def symbol: Boolean = true
  def astValue: AstValueTy = AstValueTy.Top
  def nt: BSet[Nt] = Inf
  def codeUnit: Boolean = true
  def const: BSet[String] = Inf
  def math: BSet[BigDecimal] = Inf
  def number: BSet[Number] = Inf
  def bigInt: Boolean = true
  def str: BSet[String] = Inf
  def bool: BoolTy = BoolTy.Top
  def undef: Boolean = true
  def nullv: Boolean = true
  def absent: Boolean = true
}

case class PureValueElemTy(
  clo: BSet[String] = Fin(),
  cont: BSet[Int] = Fin(),
  name: NameTy = NameTy.Bot,
  record: RecordTy = RecordTy.Bot,
  list: ListTy = ListTy.Bot,
  symbol: Boolean = false,
  astValue: AstValueTy = AstValueTy.Bot,
  nt: BSet[Nt] = Fin(),
  codeUnit: Boolean = false,
  const: BSet[String] = Fin(),
  math: BSet[BigDecimal] = Fin(),
  number: BSet[Number] = Fin(),
  bigInt: Boolean = false,
  str: BSet[String] = Fin(),
  bool: BoolTy = BoolTy.Bot,
  undef: Boolean = false,
  nullv: Boolean = false,
  absent: Boolean = false,
) extends PureValueTy
object PureValueTy extends Parser.From(Parser.pureValueTy) {
  def apply(
    clo: BSet[String] = Fin(),
    cont: BSet[Int] = Fin(),
    name: NameTy = NameTy.Bot,
    record: RecordTy = RecordTy.Bot,
    list: ListTy = ListTy.Bot,
    symbol: Boolean = false,
    astValue: AstValueTy = AstValueTy.Bot,
    nt: BSet[Nt] = Fin(),
    codeUnit: Boolean = false,
    const: BSet[String] = Fin(),
    math: BSet[BigDecimal] = Fin(),
    number: BSet[Number] = Fin(),
    bigInt: Boolean = false,
    str: BSet[String] = Fin(),
    bool: BoolTy = BoolTy.Bot,
    undef: Boolean = false,
    nullv: Boolean = false,
    absent: Boolean = false,
  ): PureValueTy = PureValueElemTy(
    clo,
    cont,
    name,
    record,
    list,
    symbol,
    astValue,
    nt,
    codeUnit,
    const,
    math,
    number,
    bigInt,
    str,
    bool,
    undef,
    nullv,
    absent,
  ).norm
  lazy val Top: PureValueTy = PureValueTopTy
  lazy val Bot: PureValueTy = PureValueElemTy()
}
