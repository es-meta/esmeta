package esmeta.state

import esmeta.state.util.*
import esmeta.util.BaseUtils.*
import esmeta.ir.Global
import esmeta.parser.ESParser

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

/** predefined enums */
val ENUM_EMPTY = Enum("empty")
val ENUM_UNRESOLVABLE = Enum("unresolvable")
val ENUM_LEXICAL = Enum("lexical")
val ENUM_INITIALIZED = Enum("initialized")
val ENUM_UNINITIALIZED = Enum("uninitialized")
val ENUM_BASE = Enum("base")
val ENUM_DERIVED = Enum("derived")
val ENUM_STRICT = Enum("strict")
val ENUM_GLOBAL = Enum("global")
val ENUM_UNLINKED = Enum("unlinked")
val ENUM_LINKING = Enum("linking")
val ENUM_LINKED = Enum("linked")
val ENUM_EVALUATING = Enum("evaluating")
val ENUM_EVALUATED = Enum("evaluated")
val ENUM_NUMBER = Enum("Number")
val ENUM_BIGINT = Enum("BigInt")
val ENUM_NORMAL = Enum("normal")
val ENUM_BREAK = Enum("break")
val ENUM_CONTINUE = Enum("continue")
val ENUM_RETURN = Enum("return")
val ENUM_THROW = Enum("throw")
val ENUM_SUSPENDED_START = Enum("suspendedStart")
val ENUM_SUSPENDED_YIELD = Enum("suspendedYield")
val ENUM_EXECUTING = Enum("executing")
val ENUM_AWAITING_RETURN = Enum("awaitingDASHreturn")
val ENUM_COMPLETED = Enum("completed")
val ENUM_PENDING = Enum("pending")
val ENUM_FULFILLED = Enum("fulfilled")
val ENUM_REJECTED = Enum("rejected")
val ENUM_FULFILL = Enum("Fulfill")
val ENUM_REJECT = Enum("Reject")

/** predefined identifiers */
val GLOBAL_RESULT = Global("RESULT")

/** predefined string */
val STR_TOP_LEVEL = "TOP_LEVEL"

/** predefined values */
val POS_INF = Infinity(pos = true)
val NEG_INF = Infinity(pos = false)
val NUMBER_POS_INF = Number(Double.PositiveInfinity)
val NUMBER_NEG_INF = Number(Double.NegativeInfinity)

def trimString(x: String, isStarting: Boolean, esParser: ESParser): String =
  val sb = new java.lang.StringBuilder
  val arr = {
    // NOTE x.codePoints is not compatible with Scala.js, Using a cross-platform alternative.
    val len = x.codePointCount(0, x.length)
    val arr = Array.ofDim[Int](len)
    var charIndex = 0
    var cpIndex = 0
    while (charIndex < x.length) {
      val cp = x.codePointAt(charIndex)
      arr(cpIndex) = cp
      charIndex += Character.charCount(cp)
      cpIndex += 1
    }
    arr
  }
  val cps = esParser.WhiteSpaceCPs ++ esParser.LineTerminatorCPs
  def find(i: Int, next: Int => Int): Int =
    if (i < 0 || i >= arr.length) i
    else if (cps contains arr(i)) find(next(i), next)
    else i
  val start = if (isStarting) find(0, _ + 1) else 0
  val end = if (isStarting) arr.length else find(arr.length - 1, _ - 1)
  arr.slice(start, end + 1).foreach(sb.appendCodePoint)
  sb.toString

/** conversion number to string */
def toStringHelper(x: Double, radix: Int = 10): String = {
  // 1. If _x_ is *NaN*, return *"NaN"*.
  if (x.isNaN) "NaN"
  // 2. If _x_ is either *+0*<sub>𝔽</sub> or *-0*<sub>𝔽</sub>, return *"0"*.
  else if (x == 0) "0"
  // 3. If _x_ < *-0*<sub>𝔽</sub>, return the string-concatenation of *"-"* and
  //    Number::toString(-_x_, _radix_).
  else if (x < 0) "-" + toStringHelper(-x, radix)
  // 4. If _x_ is *+∞*<sub>𝔽</sub>, return *"Infinity"*.
  else if (x.isPosInfinity) "Infinity"
  else {
    // 5. Let _n_, _k_, and _s_ be integers such that _k_ ≥ 1,
    //    _radix_<sup>_k_ - 1</sup> ≤ _s_ < _radix_<sup>_k_</sup>, 𝔽(_s_ ×
    //    _radix_<sup>_n_ - _k_</sup>) is _x_, and _k_ is as small as possible.
    val (n, k, s) = getShortestDigits(x, radix)
    val str = s.toString(radix)

    // 6. If _radix_ ≠ 10 or _n_ is in the inclusive interval from -5 to 21,
    //    then
    if (radix != 10 || (-5 <= n && n <= 21)) {
      if (n >= k) {
        // a. If _n_ ≥ _k_, return the string-concatenation of:
        // * the code units of the _k_ digits of the representation of _s_
        //   using radix _radix_
        str +
        // * _n_ - _k_ occurrences of the code unit 0x0030 (DIGIT ZERO)
        "0" * (n - k)
      } else if (n > 0) {
        // b. If _n_ > 0, return the string-concatenation of:
        // * the code units of the most significant _n_ digits of the
        //   representation of _s_ using radix _radix_
        str.substring(0, n) +
        // * the code unit 0x002E (FULL STOP)
        '.' +
        // * the code units of the remaining _k_ - _n_ digits of the
        //   representation of _s_ using radix _radix_
        str.substring(n)
      } else {
        // c. Assert: _n_ ≤ 0.
        // d. Return the string-concatenation of:
        // * the code unit 0x0030 (DIGIT ZERO)
        "0" +
        // * the code unit 0x002E (FULL STOP)
        "." +
        // * -_n_ occurrences of the code unit 0x0030 (DIGIT ZERO)
        "0" * -n +
        // * the code units of the _k_ digits of the representation of _s_
        //   using radix _radix_
        str
      }
    } else {
      // 7. NOTE: In this case, the input will be represented using scientific
      //    E notation, such as `1.2e+3`.
      // 8. Assert: _radix_ is 10.
      // 9. If _n_ < 0, then
      //    a. Let _exponentSign_ be the code unit 0x002D (HYPHEN-MINUS).
      // 10. Else,
      //    a. Let _exponentSign_ be the code unit 0x002B (PLUS SIGN).
      val exponentSign = if (n < 0) '-' else '+'
      if (k == 1) {
        // 11. If _k_ = 1, return the string-concatenation of:
        // * the code unit of the single digit of _s_
        str +
        // * the code unit 0x0065 (LATIN SMALL LETTER E)
        'e' +
        // * _exponentSign_
        exponentSign +
        // * the code units of the decimal representation of abs(_n_ - 1)
        math.abs(n - 1)
      } else {
        // 12. Return the string-concatenation of:
        // * the code unit of the most significant digit of the decimal
        //   representation of _s_
        str.substring(0, 1) +
        // * the code unit 0x002E (FULL STOP)
        '.' +
        // * the code units of the remaining _k_ - 1 digits of the decimal
        //   representation of _s_
        str.substring(1) +
        // * the code unit 0x0065 (LATIN SMALL LETTER E)
        'e' +
        // * _exponentSign_
        exponentSign +
        // * the code units of the decimal representation of abs(_n_ - 1)
        math.abs(n - 1)
      }
    }
  }
}

/** integers _n_, _k_, and _s_ in step 5 of Number::toString for a positive
  * finite number _x_
  *
  * If there are multiple possibilities for _s_, it chooses the value of _s_ for
  * which _s_ × _radix_<sup>_n_ - _k_</sup> is closest in value to ℝ(_x_), and
  * the even one if there are two such values, as recommended in the note of
  * Number::toString.
  */
def getShortestDigits(x: Double, radix: Int): (Int, Int, scala.math.BigInt) = {
  // find _s_ and the largest p = _n_ - _k_ such that 𝔽(_s_ ×
  // _radix_<sup>p</sup>) is _x_, where _s_ may be divisible by _radix_
  def search = {
    // ℝ(_x_) = m × 2<sup>e</sup>
    val bits = java.lang.Double.doubleToRawLongBits(x)
    val be = (bits >>> 52).toInt & 0x7ff
    val f = bits & 0xfffffffffffffL
    val (m, e) = if (be == 0) (f, -1074) else (f | (1L << 52), be - 1075)
    // in units of 2<sup>e - 2</sup>, ℝ(_x_) is v and the real numbers rounded
    // to _x_ are in [lo, hi] if m is even, and in (lo, hi) otherwise
    val v = scala.math.BigInt(m) << 2
    val hi = v + 2
    val lo = if (f == 0 && be > 1) v - 1 else v - 2
    val inclusive = m % 2 == 0
    // (a, b) such that y × 2<sup>e - 2</sup> = s × _radix_<sup>p</sup> iff
    // y × a = s × b
    val r = scala.math.BigInt(radix)
    def scale(p: Int) =
      (r.pow(0 max -p) << (0 max (e - 2)), r.pow(0 max p) << (0 max (2 - e)))
    // _n_ such that _radix_<sup>_n_ - 1</sup> ≤ ℝ(_x_) < _radix_<sup>_n_</sup>
    def lessThan(p: Int) = { val (a, b) = scale(p); v * a < b }
    var n = math.floor(math.log(x) / math.log(radix)).toInt + 1
    while (!lessThan(n)) n += 1
    while (lessThan(n - 1)) n -= 1
    // _s_ such that 𝔽(_s_ × _radix_<sup>p</sup>) is _x_ and _s_ ×
    // _radix_<sup>p</sup> is closest to ℝ(_x_) (the even one for ties), which
    // is one of the two multiples of _radix_<sup>p</sup> nearest to ℝ(_x_)
    def candidate(p: Int) =
      val (a, b) = scale(p)
      val (va, loa, hia) = (v * a, lo * a, hi * a)
      val q = va / b
      List(q, q + 1)
        .filter { s =>
          val c = s * b
          if (inclusive) loa <= c && c <= hia else loa < c && c < hia
        }
        .minByOption(s => ((s * b - va).abs, s % 2))
    // the largest p (i.e., the smallest _k_) having a candidate
    var p = n - 1
    var found = candidate(p)
    while (found.isEmpty) { p -= 1; found = candidate(p) }
    (found.get, p)
  }

  val (s0, p0) =
    // an integer less than 2<sup>53</sup> is its own shortest representation
    // because the other numbers rounded to it are not integers
    if (x.isWhole && x < (1L << 53).toDouble) (scala.math.BigInt(x.toLong), 0)
    else search
  // remove trailing zeros (e.g., _s_ = _radix_<sup>_k_</sup> by rounding up)
  var s = s0
  var p = p0
  while (s % radix == 0) { s /= radix; p += 1 }
  val k = s.toString(radix).length
  (p + k, k, s)
}

// -----------------------------------------------------------------------------
// types
// -----------------------------------------------------------------------------
type Undef = Undef.type
type Null = Null.type
