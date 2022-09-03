package esmeta.ty

import esmeta.cfg.Func
import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.analyzer.domain.*

/** pure value types (non-completion record types) */
case class PureValueTy(
  clo: BSet[String] = Fin(),
  cont: BSet[String] = Fin(),
  names: Set[String] = Set(),
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
) extends TyElem
  with Lattice[PureValueTy] {

  /** bottom check */
  def isBottom: Boolean =
    this.clo.isBottom &
    this.cont.isBottom &
    this.names.isBottom &
    this.record.isBottom &
    this.list.isBottom &
    this.symbol.isBottom &
    this.astValue.isBottom &
    this.grammar.isBottom &
    this.codeUnit.isBottom &
    this.const.isBottom &
    this.math.isBottom &
    this.number.isBottom &
    this.bigInt.isBottom &
    this.str.isBottom &
    this.bool.isBottom &
    this.undef.isBottom &
    this.nullv.isBottom &
    this.absent.isBottom

  /** partial order/subset operator */
  def <=(that: => PureValueTy): Boolean =
    this.clo <= that.clo &
    this.cont <= that.cont &
    this.names <= that.names &
    this.record <= that.record &
    this.list <= that.list &
    this.symbol <= that.symbol &
    this.astValue <= that.astValue &
    this.grammar <= that.grammar &
    this.codeUnit <= that.codeUnit &
    this.const <= that.const &
    this.math <= that.math &
    this.number <= that.number &
    this.bigInt <= that.bigInt &
    this.str <= that.str &
    this.bool <= that.bool &
    this.undef <= that.undef &
    this.nullv <= that.nullv &
    this.absent <= that.absent

  /** union type */
  def |(that: => PureValueTy): PureValueTy = PureValueTy(
    this.clo | that.clo,
    this.cont | that.cont,
    this.names | that.names,
    this.record | that.record,
    this.list | that.list,
    this.symbol | that.symbol,
    this.astValue | that.astValue, // TODO
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
  def &(that: => PureValueTy): PureValueTy = PureValueTy(
    this.clo & that.clo,
    this.cont & that.cont,
    this.names & that.names,
    this.record & that.record,
    this.list & that.list,
    this.symbol & that.symbol,
    this.astValue & that.astValue, // TODO
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
  def --(that: => PureValueTy): PureValueTy = PureValueTy(
    this.clo -- that.clo,
    this.cont -- that.cont,
    this.names -- that.names,
    this.record -- that.record,
    this.list -- that.list,
    this.symbol -- that.symbol,
    this.astValue -- that.astValue, // TODO
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

  /** get single value */
  def getSingle: Flat[APureValue] =
    (if (this.clo.isBottom) Zero else Many) |
    (if (this.cont.isBottom) Zero else Many) |
    (if (this.names.isBottom) Zero else Many) |
    (if (this.record.isBottom) Zero else Many) |
    (if (this.list.isBottom) Zero else Many) |
    (if (this.symbol.isBottom) Zero else Many) |
    (if (this.astValue.isBottom) Zero else Many) |
    (if (this.grammar.isBottom) Zero else Many) |
    (if (this.codeUnit.isBottom) Zero else Many) |
    (if (this.const.isBottom) Zero else Many) |
    (if (this.math.isBottom) Zero else Many) |
    (if (this.number.isBottom) Zero else Many) |
    (if (this.bigInt.isBottom) Zero else Many) |
    (str.getSingle.map(Str(_))) |
    (bool.getSingle.map(Bool(_))) |
    (if (this.undef.isBottom) Zero else One(Undef)) |
    (if (this.nullv.isBottom) Zero else One(Null)) |
    (if (this.absent.isBottom) Zero else One(Absent))
}
object PureValueTy extends Parser.From(Parser.pureValueTy)
