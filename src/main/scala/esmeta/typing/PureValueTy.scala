package esmeta.typing

import esmeta.state.*
import esmeta.util.*

/** pure value types (non-completion record types) */
case class PureValueTy(
  clo: CloTy = CloTy(),
  cont: ContTy = ContTy(),
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
) {

  /** union type */
  def |(that: PureValueTy): PureValueTy = PureValueTy(
    this.clo | that.clo,
    this.cont | that.cont,
    this.record | that.record,
    this.list | that.list,
    this.symbol | that.symbol,
    this.astValue | that.astValue,
    this.grammar | that.grammar,
    this.codeUnit | that.codeUnit,
    this.const | that.const,
    this.math | that.math,
    this.number | that.number,
    this.bigInt | that.bigInt,
    this.str | that.str,
    this.bool | that.bool,
    this.undef | that.undef,
    this.nullv | that.nullv,
    this.absent | that.absent,
  )

  /** intersection type */
  def &(that: PureValueTy): PureValueTy = PureValueTy(
    this.clo & that.clo,
    this.cont & that.cont,
    this.record & that.record,
    this.list & that.list,
    this.symbol & that.symbol,
    this.astValue & that.astValue,
    this.grammar & that.grammar,
    this.codeUnit & that.codeUnit,
    this.const & that.const,
    this.math & that.math,
    this.number & that.number,
    this.bigInt & that.bigInt,
    this.str & that.str,
    this.bool & that.bool,
    this.undef & that.undef,
    this.nullv & that.nullv,
    this.absent & that.absent,
  )

  /** prune type */
  def --(that: PureValueTy): PureValueTy = PureValueTy(
    this.clo -- that.clo,
    this.cont -- that.cont,
    this.record -- that.record,
    this.list -- that.list,
    this.symbol -- that.symbol,
    this.astValue -- that.astValue,
    this.grammar -- that.grammar,
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
}
