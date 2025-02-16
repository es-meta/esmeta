package esmeta.analyzer.es

import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Name, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.domain.{*, given}

/** abstract continuations */
trait AbsContDecl { self: ESAnalyzer =>

  /** abstract continuations */
  type AbsCont = AbsCont.Elem
  object AbsCont extends SetDomain[ACont]

  given Rule[AbsCont] = setRule("", " | ", "")

  /** abstract continuation elements */
  case class ACont(
    target: NodePoint[Node],
    captured: Map[Name, AbsValue],
  ) extends Printable[ACont]

  given Rule[ACont] = (app, cont) =>
    import irStringifier.given
    val ACont(target, captured) = cont
    app >> "cont<" >> target
    if (captured.nonEmpty) app >> ", " >> captured
    app >> ">"
}
