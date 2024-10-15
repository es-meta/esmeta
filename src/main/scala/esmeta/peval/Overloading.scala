package esmeta.peval

import esmeta.ir.{Func, Inst}
import esmeta.state.{State, Value}

class GetOverloads(map: (Iterable[Value], State) => Option[String]) {
  def getByArgs(args: Iterable[Value], st: State): Option[String] =
    map(args, st)
}

val NoOverloads = GetOverloads((_, _) => None)
