package esmeta.spec

import esmeta.util.BaseUtils.*
import esmeta.spec.*
import Symbol.*

class StringifierTinyTest extends SpecTest {
  val name: String = "specStringifierTest"

  // test helper
  def test[T <: SpecElem](desc: String)(cases: (T, String)*): Unit =
    check(
      desc,
      cases.foreach { case (input, expected) =>
        val result = input.toString
        if (result != expected) {
          println(s"[FAILED: Symbol]")
          println(s" - result  : $result")
          println(s" - expected: $expected")
          assert(result == expected)
        }
      },
    )

  // registration
  def init: Unit = {
    // pre-defined values
    val ntArgs = List(
      NtArg(NtArg.Kind.True, "Await"),
      NtArg(NtArg.Kind.False, "Yield"),
      NtArg(NtArg.Kind.Pass, "For"),
    )
    val nt: Nonterminal = Nonterminal("Identifier", Nil, false)
    val symbols = List(Terminal("{"), Terminal("}"))

    test("Symbol")(
      Terminal("{") -> "`{`",
      Nonterminal(
        "Identifier",
        ntArgs,
        true,
      ) -> "Identifier[+Await, ~Yield, ?For]?",
      Nonterminal("Identifier", Nil, false) -> "Identifier",
      ButNot(nt, List(nt)) -> "Identifier but not Identifier",
      Lookahead(
        true,
        List(symbols, symbols),
      ) -> "[lookahead < {`{` `}`, `{` `}`}]",
      Lookahead(
        false,
        List(symbols, symbols),
      ) -> "[lookahead <! {`{` `}`, `{` `}`}]",
      Empty -> "[empty]",
      NoLineTerminator -> "[no LineTerminator here]",
      Unicode("LT") -> "<LT>",
      UnicodeAny -> "<UnicodeAny>",
      UnicodeIdStart -> "<UnicodeIdStart>",
      UnicodeIdContinue -> "<UnicodeIdContinue>",
      UnicodeLeadSurrogate -> "<UnicodeLeadSurrogate>",
      UnicodeTrailSurrogate -> "<UnicodeTrailSurrogate>",
      NotCodePoint -> "<NotCodePoint>",
      CodePoint -> "<CodePoint>",
      HexLeadSurrogate -> "<HexLeadSurrogate>",
      HexTrailSurrogate -> "<HexTrailSurrogate>",
      HexNonSurrogate -> "<HexNonSurrogate>",
      NonUnicodeModeDecimalEscape -> "<NonUnicodeModeDecimalEscape>",
    )

    test("NtArg")(
      // TODO
    )

    test("NtArg.Kind")(
      // TODO
    )

    test("RhsCond")(
      // TODO
    )

    test("Rhs")(
      // TODO
    )

    test("Lhs")(
      // TODO
    )

    test("Production")(
      // TODO
    )

    test("Production.Kind")(
      // TODO
    )

    test("Grammar")(
      // TODO
    )

    // EXAMPLE test("Inst")(
    //   IExpr(EINum(4)) -> "4i",
    // )
  }

  init
}
