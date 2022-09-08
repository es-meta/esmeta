package esmeta.ty

import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.state.*
import esmeta.util.*

/** value types */
case class ValueTy(
  comp: CompTy,
  pureValue: PureValueTy,
  subMap: SubMapTy,
) extends Ty
  with Lattice[ValueTy] {
  import ValueTy.*

  /** bottom check */
  def isBottom: Boolean = (this eq Bot) | (
    this.comp.isBottom &
    this.pureValue.isBottom &
    this.subMap.isBottom
  )

  /** partial order/subset operator */
  def <=(that: => ValueTy): Boolean = (this eq that) | (
    this.comp <= that.comp &
    this.pureValue <= that.pureValue &
    this.subMap <= that.subMap
  )

  /** union type */
  def |(that: => ValueTy): ValueTy =
    if (this eq that) this
    else
      ValueTy(
        this.comp | that.comp,
        this.pureValue | that.pureValue,
        this.subMap | that.subMap,
      )

  /** intersection type */
  def &(that: => ValueTy): ValueTy =
    if (this eq that) this
    else
      ValueTy(
        this.comp & that.comp,
        this.pureValue & that.pureValue,
        this.subMap & that.subMap,
      )

  /** prune type */
  def --(that: => ValueTy): ValueTy =
    if (that.isBottom) this
    else
      ValueTy(
        this.comp -- that.comp,
        this.pureValue -- that.pureValue,
        this.subMap -- that.subMap,
      )

  /** get single value */
  def getSingle: Flat[AValue] =
    this.comp.getSingle |
    this.pureValue.getSingle |
    this.subMap.getSingle

  /** completion check */
  def isCompletion: Boolean =
    !comp.isBottom &
    pureValue.isBottom &
    subMap.isBottom

  /** getters */
  def normal: PureValueTy = comp.normal
  def abrupt: Boolean = comp.abrupt
  def clo: BSet[String] = pureValue.clo
  def cont: BSet[Int] = pureValue.cont
  def names: Set[String] = pureValue.names
  def record: RecordTy = pureValue.record
  def list: ListTy = pureValue.list
  def symbol: Boolean = pureValue.symbol
  def astValue: AstValueTy = pureValue.astValue
  def grammar: BSet[Grammar] = pureValue.grammar
  def codeUnit: Boolean = pureValue.codeUnit
  def const: Set[String] = pureValue.const
  def math: BSet[BigDecimal] = pureValue.math
  def number: BSet[Number] = pureValue.number
  def bigInt: Boolean = pureValue.bigInt
  def str: BSet[String] = pureValue.str
  def bool: Set[Boolean] = pureValue.bool
  def undef: Boolean = pureValue.undef
  def nullv: Boolean = pureValue.nullv
  def absent: Boolean = pureValue.absent
}
object ValueTy {
  def apply(
    comp: CompTy = CompTy.Bot,
    normal: PureValueTy = PureValueTy.Bot,
    abrupt: Boolean = false,
    pureValue: PureValueTy = PureValueTy.Bot,
    clo: BSet[String] = Fin(),
    cont: BSet[Int] = Fin(),
    names: Set[String] = Set(),
    record: RecordTy = RecordTy.Bot,
    list: ListTy = ListTy.Bot,
    symbol: Boolean = false,
    astValue: AstValueTy = AstValueTy.Bot,
    grammar: BSet[Grammar] = Fin(),
    codeUnit: Boolean = false,
    const: Set[String] = Set(),
    math: BSet[BigDecimal] = Fin(),
    number: BSet[Number] = Fin(),
    bigInt: Boolean = false,
    str: BSet[String] = Fin(),
    bool: Set[Boolean] = Set(),
    undef: Boolean = false,
    nullv: Boolean = false,
    absent: Boolean = false,
    subMap: SubMapTy = SubMapTy.Bot,
  ): ValueTy = ValueTy(
    comp = comp | CompTy(normal, abrupt),
    pureValue = pureValue | PureValueTy(
      clo,
      cont,
      names,
      record,
      list,
      symbol,
      astValue,
      grammar,
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
    ),
    subMap = subMap,
  )
  val Bot: ValueTy = ValueTy()
}
