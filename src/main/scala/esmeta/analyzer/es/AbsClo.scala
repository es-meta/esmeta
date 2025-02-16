package esmeta.analyzer.es

import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Name, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.domain.{*, given}

/** abstract closures */
trait AbsCloDecl { self: ESAnalyzer =>

  /** abstract closures */
  type AbsClo = AbsClo.Elem
  object AbsClo extends SetDomain[AClo]

  given Rule[AbsClo] = setRule("", " | ", "")

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
  given Rule[AClo] = (app, clo) => {
    import irStringifier.given
    val AClo(func, captured) = clo
    app >> "clo<" >> func.name
    if (!captured.isEmpty) app >> ", " >> captured.toList
    app >> ">"
  }
}
