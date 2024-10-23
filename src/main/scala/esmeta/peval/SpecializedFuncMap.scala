package esmeta.peval

import esmeta.ir.{Func, Inst}
import esmeta.state.{State, Value}

import scala.collection.immutable.HashMap

class SpecializedFuncs(
  map: Map[String, (Iterable[Value], State) => Option[String]],
) {
  def getByArgs(
    funcName: String,
    args: Iterable[Value],
    st: State,
  ): Option[String] =
    map.get(funcName).flatMap(_(args, st))

}

object SpecializedFuncs {
  val EMPTY = SpecializedFuncs(Map.empty)
}
