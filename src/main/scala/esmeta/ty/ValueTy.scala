package esmeta.ty

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

  /** bottom check */
  def isBottom: Boolean =
    this.comp.isBottom &
    this.pureValue.isBottom &
    this.subMap.isBottom

  /** partial order/subset operator */
  def <=(that: => ValueTy): Boolean =
    this.comp <= that.comp &
    this.pureValue <= that.pureValue &
    this.subMap <= that.subMap

  /** union type */
  def |(that: => ValueTy): ValueTy = ValueTy(
    this.comp | that.comp,
    this.pureValue | that.pureValue,
    this.subMap | that.subMap,
  )

  /** intersection type */
  def &(that: => ValueTy): ValueTy = ValueTy(
    this.comp & that.comp,
    this.pureValue & that.pureValue,
    this.subMap & that.subMap,
  )

  /** prune type */
  def --(that: => ValueTy): ValueTy = ValueTy(
    this.comp -- that.comp,
    this.pureValue -- that.pureValue,
    this.subMap -- that.subMap,
  )

  /** completion check */
  def isCompletion: Boolean =
    !comp.isBottom &
    pureValue.isBottom &
    subMap.isBottom

  /** getters */
  def normal: PureValueTy = comp.normal
  def abrupt: Boolean = comp.abrupt
  def clo: BSet[String] = pureValue.clo
  def cont: BSet[String] = pureValue.cont
  def record: RecordTy = pureValue.record
  def list: ListTy = pureValue.list
  def symbol: Boolean = pureValue.symbol
  def astValue: BSet[String] = pureValue.astValue
  def grammar: BSet[Grammar] = pureValue.grammar
  def codeUnit: Boolean = pureValue.codeUnit
  def const: Set[String] = pureValue.const
  def math: Boolean = pureValue.math
  def number: Boolean = pureValue.number
  def bigInt: Boolean = pureValue.bigInt
  def str: BSet[String] = pureValue.str
  def bool: Set[Boolean] = pureValue.bool
  def undef: Boolean = pureValue.undef
  def nullv: Boolean = pureValue.nullv
  def absent: Boolean = pureValue.absent
}
object ValueTy {
  def apply(
    comp: CompTy = CompTy(),
    normal: PureValueTy = PureValueTy(),
    abrupt: Boolean = false,
    pureValue: PureValueTy = PureValueTy(),
    clo: BSet[String] = Fin(),
    cont: BSet[String] = Fin(),
    record: RecordTy = RecordTy(),
    list: ListTy = ListTy(),
    symbol: Boolean = false,
    astValue: BSet[String] = Fin(),
    grammar: BSet[Grammar] = Fin(),
    codeUnit: Boolean = false,
    const: Set[String] = Set(),
    math: Boolean = false,
    number: Boolean = false,
    bigInt: Boolean = false,
    str: BSet[String] = Fin(),
    bool: Set[Boolean] = Set(),
    undef: Boolean = false,
    nullv: Boolean = false,
    absent: Boolean = false,
    subMap: SubMapTy = SubMapTy(),
  ): ValueTy = ValueTy(
    comp = comp | CompTy(normal, abrupt),
    pureValue = pureValue | PureValueTy(
      clo,
      cont,
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
}
