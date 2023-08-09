package esmeta.analyzer

import esmeta.analyzer.util.*
import esmeta.cfg.{Func, Node}
import esmeta.util.BaseUtils.*

case class Unreachable(
  func: Func,
  node: Node,
  cause: Option[Node],
) {
  override def toString: String = toString(true, false, false)
  def toString(
    detail: Boolean = false,
    line: Boolean = false,
    asite: Boolean = false,
  ): String =
    val stringifier = Unreachable.getStringifier(detail, line, asite)
    import stringifier.urRule
    stringify(this)
}

object Unreachable {
  val getStringifier = cached[(Boolean, Boolean, Boolean), Stringifier] {
    Stringifier(_, _, _)
  }
}
