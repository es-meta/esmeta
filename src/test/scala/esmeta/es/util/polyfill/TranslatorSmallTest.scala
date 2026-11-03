package esmeta.es.util.polyfill

import esmeta.*
import esmeta.error.NotSupported as NotSupportedError
import esmeta.lang.*

/** tests for the translation of metalanguage into polyfill code */
class TranslatorSmallTest extends PolyfillTest {
  val name: String = "polyfillTranslatorTest"

  // registration
  def init: Unit = {
    import PolyfillTest.*

    // -------------------------------------------------------------------------
    // loops
    // -------------------------------------------------------------------------
    checkEqual("ForEachIntegerStep")(
      step(
        "for each integer _x_ such that 2 ≤ _x_ ≤ 6, in ascending order, let _y_ be _x_.",
      ) -> "for (var x = 2; x <=6; x++)",
      step(
        "for each integer _x_ such that 2 < _x_ < 6, in ascending order, let _y_ be _x_.",
      ) -> "for (var x = 2 + 1; x <6; x++)",
      // a descending loop starts from the upper bound
      step(
        "for each integer _x_ such that 2 ≤ _x_ ≤ 6, in descending order, let _y_ be _x_.",
      ) -> "for (var x = 6; x >=2; x--)",
      step(
        "for each integer _x_ such that 2 < _x_ < 6, in descending order, let _y_ be _x_.",
      ) -> "for (var x = 6 - 1; x >2; x--)",
    )

    checkEqual("ForEachStep")(
      step("for each element _x_ of _l_, let _y_ be _x_.") ->
      "for (var _x0 = 0; _x0 < l.length; _x0++)",
      // a loop in reverse List order starts from the last element
      step(
        "for each element _x_ of _l_, in reverse List order, let _y_ be _x_.",
      ) ->
      "for (var _x0 = l.length - 1; _x0 >=0; _x0--)",
    )

    // -------------------------------------------------------------------------
    // conditions
    // -------------------------------------------------------------------------
    checkEqual("HasFieldCondition")(
      // fields and slots are own properties, never inherited ones
      cond("_x_ has a [[MapData]] internal slot") ->
      "(IN__HasField(x, \"MapData\"))",
      cond("_x_ does not have a [[MapData]] internal slot") ->
      "!(IN__HasField(x, \"MapData\"))",
    )

    checkEqual("TypeCheck")(
      typeCheck("record[object]") -> "AO__IsObject(x)",
      typeCheck("record[symbol]") -> "typeof x === \"symbol\"",
      // ECMAScript code never passes a Private Name to a built-in
      typeCheck("record[privatename]") -> "false",
    )

    check("TypeCheck (unknown record)") {
      // an unknown record type must fail, not silently be false
      val e = intercept[NotSupportedError](typeCheck("record[foo]"))
      assert(e.reasonPath.contains("TypeCheck"))
    }

    // -------------------------------------------------------------------------
    // literals
    // -------------------------------------------------------------------------
    checkEqual("StringLiteral")(
      // quotes, backslashes and line terminators are escaped
      expr(StringLiteral("a\"b\\c\nd\u2028")) -> "\"a\\\"b\\\\c\\nd\\u2028\"",
    )
  }

  init
}
