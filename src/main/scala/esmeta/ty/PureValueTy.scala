package esmeta.ty

import esmeta.cfg.Func
import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

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
}
object PureValueTy extends Parser.From(Parser.pureValueTy)
