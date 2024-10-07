package esmeta.peval.util

import esmeta.peval.*
import esmeta.peval.pstate.*
import esmeta.state.*

import esmeta.util.{BSet, Fin, Inf}
import esmeta.util.BaseUtils.{cached}

class HeapImpact(heap: PHeap) {

  type PV = Predict[Value]

  def ofList(pvs: Iterable[PV]): BSet[Addr] =
    pvs.map(of).foldLeft[BSet[Addr]](Fin())(_ || _)

  lazy val of: PV => BSet[Addr] = cached[PV, BSet[Addr]] {
    case Unknown => Inf
    case Known(v) =>
      v match
        case addr: Addr =>
          Fin(addr) || heap(addr)
            .map(_ match
              case PRecordObj(_, map) =>
                map.values
                  .map {
                    case Unknown          => Inf
                    case Known(_: Uninit) => Fin()
                    case Known(v: Value)  => of(Known(v))
                  }
                  .reduce(_ || _)
              case PMapObj(map)     => ???
              case PListObj(values) => ofList(values)
              case PYetObj(_, _)    => Fin(),
            )
            .getOrElse(Inf)
        case Clo(_, captured)  => ofList(captured.map(_._2).map(Known.apply))
        case PClo(_, captured) => ofList(captured.map(_._2))
        case Cont(func, captured, callStack)  => ???
        case PCont(func, captured, callStack) => ???
        case AstValue(_) | GrammarSymbol(_, _) | Math(_) | Infinity(_) |
            Enum(_) | CodeUnit(_) | Number(_) | BigInt(_) | Str(_) | Bool(_) |
            Undef | Null =>
          Fin()
  }

}
