package esmeta.peval

import esmeta.ir.{Func, Inst}
import esmeta.state.{Value}

class GetOverloads(map: (Iterable[Value]) => Option[String]) {
  def apply(args: Iterable[Value]): Option[String] = map(args)
}

val NoOverloads = GetOverloads((_) => None)
