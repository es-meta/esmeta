package esmeta.peval.pstate

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
import esmeta.peval.domain.*
import esmeta.state.*

/** IR PHeap for partial Evaluation. similar to state/PHeap.scala */
case class PHeap(
  val map: MMap[Addr, PObj] = MMap(),
  var size: Int = 0,
) extends StateElem {

  /** getter */
  def apply(addr: Addr): PObj = map.getOrElse(addr, throw UnknownAddr(addr))
  def apply(addr: Addr, field: Value): PValue = apply(addr)(field)

  /** setter */
  def update(addr: Addr, field: Value, value: PValue): Unit =
    apply(addr).update(field, value)

  /** existence check */
  def exists(addr: Addr, field: Value): Boolean = apply(addr).exists(field)

  /** expand */
  def expand(addr: Addr, field: Value): Unit = apply(addr).expand(field)

  /** delete */
  def delete(addr: Addr, key: Value): Unit = apply(addr).delete(key)

  /** push */
  def push(addr: Addr, value: PValue, front: Boolean): Unit =
    apply(addr).push(value, front)

  /** pops */
  def pop(addr: Addr, front: Boolean): PValue = apply(addr).pop(front)

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
            case GrammarSymbol(name, params) =>
              PValue.V(v, EGrammarSymbol(name, params))
            case Math(decimal)  => PValue.V(v, EMath(decimal))
            case Infinity(pos)  => PValue.V(v, EInfinity(pos))
            case Enum(name)     => PValue.V(v, EEnum(name))
            case CodeUnit(c)    => PValue.V(v, ECodeUnit(c))
            case Number(double) => PValue.V(v, ENumber(double))
            case BigInt(bigInt) => PValue.V(v, EBigInt(bigInt))
            case Str(str)       => PValue.V(v, EStr(str))
            case Bool(bool)     => PValue.V(v, EBool(bool))
            case Undef          => PValue.V(v, EUndef())
            case Null           => PValue.V(v, ENull())

        }),
    )

  /** record allocations */
  def allocRecord(
    tname: String,
    pairs: Iterable[(String, PValue)] = Nil,
  )(using CFG): Addr = alloc(PRecordObj(tname, pairs))

  /** map allocations */
  def allocMap(pairs: Iterable[(Value, PValue)]): Addr = alloc(PMapObj(pairs))

  /** list allocations */
  def allocList(vs: Iterable[PValue]): Addr = alloc(PListObj(vs.toVector))

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
