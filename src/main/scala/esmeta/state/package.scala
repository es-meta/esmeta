package esmeta.state

import esmeta.state.util.*
import esmeta.util.BaseUtils.*
import esmeta.ir.Global
import java.math.MathContext.{UNLIMITED, DECIMAL128}

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
    cached[(Boolean, Boolean), Stringifier] { Stringifier(_, _) }
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
val POS_INF = Infinity(pos = true)
val NEG_INF = Infinity(pos = false)
val NUMBER_POS_INF = Number(Double.PositiveInfinity)
val NUMBER_NEG_INF = Number(Double.NegativeInfinity)

/** conversion number to string */
def toStringHelper(x: Double, radix: Int = 10): String = {
  // get sign
  def getSign(n: Int): Char = if (n - 1 > 0) '+' else '-'

  // get string of number
  def getStr(number: scala.math.BigInt, radix: Int): String =
    var str = ""
    var s = number
    while (s > 0) { str += getRadixString(s % radix); s /= radix }
    str.reverse

  // get radix string of number
  def getRadixString(d: scala.math.BigInt): String =
    if (d < 10) d.toString else ('a' + (d - 10)).toChar.toString

  // 1. If _x_ is *NaN*, return the String *"NaN"*.
  if (x.isNaN) "NaN"
  // 2. If _x_ is *+0*<sub>𝔽</sub> or *-0*<sub>𝔽</sub>, return the String
  //    *"0"*.
  else if (x == 0) "0"
  // 3. If _x_ < *-0*<sub>𝔽</sub>, return the string-concatenation of *"-"* and
  //    toStringHelper(-_x_).
  else if (x < 0) "-" + toStringHelper(-x, radix)
  // 4. If _x_ is *+∞*<sub>𝔽</sub>, return the String *"Infinity"*.
  else if (x.isPosInfinity) "Infinity"
  else {
    // 5. Otherwise, let _n_, _k_, and _s_ be integers such that _k_ >= 1,
    //    _radix_<sup>_k_ - 1</sup> <= _s_ < _radix_<sup>_k_</sup>, 𝔽(_s_ *
    //    _radix_<sup>_n_ - _k_</sup>) is _x_, and _k_ is as small as possible.
    val (n: Int, k: Int, s: scala.math.BigInt) =
      var S = BigDecimal(x, UNLIMITED)
      var N = 0
      while (S % radix == 0) { S /= radix; N += 1 }
      while (S % 1 != 0) { S *= radix; N -= 1 }
      var RK = BigDecimal(radix, UNLIMITED)
      var K = 1
      while (S >= RK) { RK *= radix; K += 1 }
      (N + K, K, S.toBigInt)

    if (k <= n && n <= 21) {
      // * the code units of the _k_ digits of the decimal representation of
      //   _s_ (in order, with no leading zeroes)
      getStr(s, radix) +
      // * _n_ - _k_ occurrences of the code unit 0x0030 (DIGIT ZERO)
      "0" * (n - k)
    } else if (0 < n && n <= 21) {
      val str = getStr(s, radix)
      // * the code units of the most significant _n_ digits of the decimal
      //   representation of _s_
      str.substring(0, n) +
      // * the code unit 0x002E (FULL STOP)
      '.' +
      // * the code units of the remaining _k_ - _n_ digits of the decimal
      //   representation of _s_
      str.substring(n)
    } else if (-6 < n && n <= 0) {
      // * the code unit 0x0030 (DIGIT ZERO)
      "0" +
      // * the code unit 0x002E (FULL STOP)
      "." +
      // * -_n_ occurrences of the code unit 0x0030 (DIGIT ZERO)
      "0" * -n +
      // * the code units of the _k_ digits of the decimal representation of _s_
      getStr(s, radix)
    } else if (k == 1) {
      // * the code unit of the single digit of _s_
      getStr(s, radix) +
      // * the code unit 0x0065 (LATIN SMALL LETTER E)
      "e" +
      // * the code unit 0x002B (PLUS SIGN) or the code unit 0x002D
      //   (HYPHEN-MINUS) according to whether _n_ - 1 is positive or negative
      getSign(n) +
      // * the code units of the decimal representation of the integer abs(_n_
      //   - 1) (with no leading zeroes)
      math.abs(n - 1)
    } else {
      val str = getStr(s, radix)
      // * the code units of the most significant digit of the decimal
      //   representation of _s_
      str.substring(0, 1) +
      // * the code unit 0x002E (FULL STOP)
      '.' +
      // * the code units of the remaining _k_ - 1 digits of the decimal
      //   representation of _s_
      str.substring(1) +
      // * the code unit 0x0065 (LATIN SMALL LETTER E)
      'e' +
      // * the code unit 0x002B (PLUS SIGN) or the code unit 0x002D
      //   (HYPHEN-MINUS) according to whether _n_ - 1 is positive or negative
      getSign(n) +
      // * the code units of the decimal representation of the integer abs(_n_
      //   - 1) (with no leading zeroes)
      math.abs(n - 1)
    }
  }
}

// -----------------------------------------------------------------------------
// types
// -----------------------------------------------------------------------------
type Undef = Undef.type
type Null = Null.type
type Absent = Absent.type
