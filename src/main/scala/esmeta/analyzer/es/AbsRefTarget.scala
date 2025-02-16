package esmeta.analyzer.es

import esmeta.ir.*

/** abstract IR reference target */
trait AbsRefTargetDecl { self: ESAnalyzer =>

  enum AbsRefTarget:
    case AbsId(id: Var)
    case AbsField(base: AbsValue, field: AbsValue)
    override def toString: String = this match
      case AbsId(id)             => s"$id"
      case AbsField(base, field) => s"$base[$field]"
}
