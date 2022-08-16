package esmeta.state

import esmeta.state.util.*
import esmeta.util.BaseUtils.*
import esmeta.ir.Global

/** IR state elements */
trait StateElem {
  override def toString: String = toString(true, false)

  /** stringify with options */
  def toString(detail: Boolean = true, location: Boolean = false): String =
    val stringifier = StateElem.getStringifier(detail, location)
    import stringifier.elemRule
    stringify(this)
}
object StateElem {
  val getStringifier =
    cached[(Boolean, Boolean), Stringifier] { new Stringifier(_, _) }
}

/** predefined constants */
val CONST_EMPTY = Const("empty")
val CONST_UNRESOLVABLE = Const("unresolvable")
val CONST_LEXICAL = Const("lexical")
val CONST_INITIALIZED = Const("initialized")
val CONST_UNINITIALIZED = Const("uninitialized")
val CONST_BASE = Const("base")
val CONST_DERIVED = Const("derived")
val CONST_STRICT = Const("strict")
val CONST_GLOBAL = Const("global")
val CONST_UNLINKED = Const("unlinked")
val CONST_LINKING = Const("linking")
val CONST_LINKED = Const("linked")
val CONST_EVALUATING = Const("evaluating")
val CONST_EVALUATED = Const("evaluated")
val CONST_NUMBER = Const("Number")
val CONST_BIGINT = Const("BigInt")
val CONST_NORMAL = Const("normal")
val CONST_BREAK = Const("break")
val CONST_CONTINUE = Const("continue")
val CONST_RETURN = Const("return")
val CONST_THROW = Const("throw")
val CONST_SUSPENDED_START = Const("suspendedStart")
val CONST_SUSPENDED_YIELD = Const("suspendedYield")
val CONST_EXECUTING = Const("executing")
val CONST_AWAITING_RETURN = Const("awaitingDASHreturn")
val CONST_COMPLETED = Const("completed")
val CONST_PENDING = Const("pending")
val CONST_FULFILLED = Const("fulfilled")
val CONST_REJECTED = Const("rejected")
val CONST_FULFILL = Const("Fulfill")
val CONST_REJECT = Const("Reject")

/** predefined identifiers */
val GLOBAL_RESULT = Global("RESULT")

/** predefined string */
val STR_TOP_LEVEL = "TOP_LEVEL"

/** predefined values */
val POS_INF = Number(Double.PositiveInfinity)
val NEG_INF = Number(Double.NegativeInfinity)

/** conversion number to string */
def toStringHelper(m: Double, radix: Int = 10): String = {
  // get sign
  def getSign(n: Int): Char = if (n - 1 > 0) '+' else '-'

  // get string of number
  def getStr(number: Long, radix: Int): String =
    var str = ""
    var sLong = number
    while (sLong > 0) { str += getRadixString(sLong % radix); sLong /= radix }
    str.reverse

  // get radix string of number
  def getRadixString(d: Long): String =
    if (d < 10) d.toString else ('a' + (d - 10)).toChar.toString

  if (m.isNaN) "NaN"
  else if (m == 0) "0"
  else if (m < 0) "-" + toStringHelper(-m, radix)
  else if (m.isPosInfinity) "Infinity"
  else {
    var s = BigDecimal(m)
    var n = 0
    while (s % radix == BigDecimal(0) || s % 1 != BigDecimal(0)) {
      if (s % radix == BigDecimal(0)) { s /= radix; n += 1 }
      else { s *= radix; n -= 1 }
    }
    while (
      (((s - (s % radix)) / radix) * BigDecimal(radix).pow(n + 1)).toDouble == m
    ) {
      s = (s - (s % radix)) / radix
      n = n + 1
    }
    var sLong = s.toLong
    var k = 0
    while (s >= BigDecimal(1)) { s /= radix; k += 1 }
    n += k
    if (k <= n && n <= 21) {
      getStr(sLong, radix) + ("0" * (n - k))
    } else if (0 < n && n <= 21) {
      val str = getStr(sLong, radix)
      str.substring(0, n) + '.' + str.substring(n)
    } else if (-6 < n && n <= 0) {
      "0." + ("0" * (-n)) + getStr(sLong, radix)
    } else if (k == 1) {
      getStr(sLong, radix) + "e" + getSign(n) + math.abs(n - 1).toString
    } else {
      val str = getStr(sLong, radix)
      str.substring(0, 1) + '.' + str
        .substring(1) + 'e' + getSign(n) + math.abs(n - 1).toString
    }
  }
}
