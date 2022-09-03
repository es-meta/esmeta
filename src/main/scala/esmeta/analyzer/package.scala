package esmeta.analyzer

import esmeta.analyzer.util.*
import esmeta.error.*
import esmeta.util.BaseUtils.*

/** analyzer elements */
trait AnalyzerElem {
  override def toString: String = toString(true, false, false)

  /** stringify with options */
  def toString(
    detail: Boolean = false,
    line: Boolean = false,
    asite: Boolean = false,
  ): String =
    val stringifier = AnalyzerElem.getStringifier(detail, line, asite)
    import stringifier.elemRule
    stringify(this)
}
object AnalyzerElem {
  val getStringifier = cached[(Boolean, Boolean, Boolean), Stringifier] {
    Stringifier(_, _, _)
  }
}

/** exploded */
def exploded(msg: String): Nothing = throw AnalysisImprecise(msg)

/** not supported */
def notSupported(msg: String): Nothing = throw NotSupported(msg)
