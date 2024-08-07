package esmeta.peval

import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.*
import esmeta.es.builtin.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.state.{Addr, Obj, StateElem}
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}

// TODO sort imports
import esmeta.state.Value
import esmeta.state.DynamicAddr
import esmeta.state.NamedAddr
import esmeta.state.Clo
import esmeta.state.Cont
import esmeta.state.AstValue
import esmeta.state.GrammarSymbol
import esmeta.state.Infinity
import esmeta.state.CodeUnit
import esmeta.state.Str
import esmeta.state.Bool
import esmeta.state.Undef

/** IR PHeap for partial Evaluation. similar to state/PHeap.scala */
case class PHeap(
  val map: MMap[Addr, PObj] = MMap(),
  var size: Int = 0,
) extends StateElem {

  /** getter */
  def apply(addr: Addr): PObj = map.getOrElse(addr, throw UnknownAddr(addr))
  def apply(addr: Addr, field: Value): Expr = apply(addr)(field)

  /** setter */
  def update(addr: Addr, field: Value, value: Expr): Unit =
    apply(addr).update(field, value)

  /** existence check */
  def exists(addr: Addr, field: Value): Boolean = apply(addr).exists(field)

  /** expand */
  def expand(addr: Addr, field: Value): Unit = apply(addr).expand(field)

  /** delete */
  def delete(addr: Addr, key: Value): Unit = apply(addr).delete(key)

  /** push */
  def push(addr: Addr, value: Expr, front: Boolean): Unit =
    apply(addr).push(value, front)

  /** pops */
  def pop(addr: Addr, front: Boolean): Expr = apply(addr).pop(front)

  /** copy */
  def copy(addr: Addr): Addr = alloc(apply(addr).copied)

  /** keys */
  def keys(addr: Addr, intSorted: Boolean): Addr =
    allocList(
      apply(addr)
        .keys(intSorted)
        .map((v) => {
          v match
            case NamedAddr(name)                 => ???
            case DynamicAddr(long)               => ???
            case Clo(func, captured)             => ???
            case Cont(func, captured, callStack) => ???
            case AstValue(ast)                   => ???
            case GrammarSymbol(name, params)     => EGrammarSymbol(name, params)
            case esmeta.state.Math(decimal)      => EMath(decimal)
            case Infinity(pos)                   => EInfinity(pos)
            case esmeta.state.Enum(name)         => EEnum(name)
            case CodeUnit(c)                     => ECodeUnit(c)
            case esmeta.state.Number(double)     => ENumber(double)
            case esmeta.state.BigInt(bigInt)     => EBigInt(bigInt)
            case Str(str)                        => EStr(str)
            case Bool(bool)                      => EBool(bool)
            case Undef                           => EUndef()
            case esmeta.state.Null               => ENull()

        }),
    )

  /** record allocations */
  def allocRecord(
    tname: String,
    pairs: Iterable[(String, Expr)] = Nil,
  )(using CFG): Addr = alloc(PRecordObj(tname, pairs))

  /** map allocations */
  def allocMap(pairs: Iterable[(Value, Expr)]): Addr = alloc(PMapObj(pairs))

  /** list allocations */
  def allocList(vs: Iterable[Expr]): Addr = alloc(PListObj(vs.toVector))

  // allocation helper
  private def alloc(obj: PObj): Addr = {
    val newAddr = DynamicAddr(size)
    map += newAddr -> obj
    size += 1
    newAddr
  }

  /** copied */
  def copied: PHeap = PHeap(MMap.from(map.toList.map { _ -> _.copied }), size)
}
