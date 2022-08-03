package esmeta.analyzer

import esmeta.analyzer.util.*
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
  val getStringifier =
    cached[(Boolean, Boolean, Boolean), Stringifier] {
      new Stringifier(_, _, _)
    }
}

/** options */
var USE_REPL: Boolean = false
var USE_GC: Boolean = false
var INF_SENS: Boolean = true
var IR_SENS: Boolean = true
var TYPE_SENS: Boolean = false
var ANALYZE_TIMEOUT: Long = 20

/** (i, j) for loop sensitivity */
var LOOP_ITER: Int = 100
var LOOP_DEPTH: Int = 20

/** k for call-site sensitivity */
var JS_CALL_DEPTH: Int = 20
var IR_CALL_DEPTH: Int = 50
