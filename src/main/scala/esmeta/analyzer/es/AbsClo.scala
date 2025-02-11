package esmeta.analyzer.es

import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.Name
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.domain.*, Lattice.*, BSet.*, Flat.*

/** abstract primitive values */
trait AbsCloDecl { self: ESAnalyzer =>

  /** abstract closures */
  type AbsClo = AbsClo.Elem
  object AbsClo extends SetDomain[AClo] {
    given rule: Rule[AbsClo] = setRule
  }

  /** abstract closure elements */
  case class AClo(
    func: Func,
    captured: Map[Name, AbsValue],
  ) extends Printable[AClo]
  object AClo {
    def apply(clo: Clo): AClo =
      val Clo(func, captured) = clo
      val newCaptured = captured.map((x, v) => x -> AbsValue(v)).toMap
      AClo(func, newCaptured)
  }
  given Rule[AClo] = (app, clo) => app >> "clo<" >> clo.func.name >> ">"
  given Ordering[AClo] = Ordering.by(_.func.id)
}
