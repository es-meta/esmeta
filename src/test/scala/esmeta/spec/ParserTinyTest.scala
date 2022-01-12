package esmeta.spec

import esmeta.util.BaseUtils.*
import esmeta.spec.*
import Symbol.*

class ParserTinyTest extends SpecTest {
  val name: String = "specParserTest"

  // registration
  def init: Unit = {
    // /////////////////////////////////////////////////////////////////////////
    // Grammar
    // /////////////////////////////////////////////////////////////////////////
    // pre-defined values
    val ntArgs = List(
      NtArg(NtArg.Kind.True, "Await"),
      NtArg(NtArg.Kind.False, "Yield"),
      NtArg(NtArg.Kind.Pass, "For"),
    )
    val nt: Nonterminal = Nonterminal("Identifier", Nil, false)
    val symbols = List(Terminal("{"), Terminal("}"))

    checkEquals("Symbol")(
      Symbol("`{`") -> Terminal("{"),
      Symbol("Identifier[+Await, ~Yield, ?For]?") ->
        Nonterminal("Identifier", ntArgs, true),
      Symbol("Identifier") -> Nonterminal("Identifier", Nil, false),
      Symbol("Identifier but not Identifier") -> ButNot(nt, List(nt)),
      Symbol("Identifier [> but only if MV of |Identifier|> 0x10FFFF]") ->
        ButOnlyIf(nt, "MV", "> 0x10FFFF"),
      Symbol("[lookahead < {`{` `}`, `{` `}`}]") ->
        Lookahead(true, List(symbols, symbols)),
      Symbol("[lookahead <! {`{` `}`, `{` `}`}]") -> Lookahead(
        false,
        List(symbols, symbols),
      ),
      Symbol("[empty]") -> Empty,
      Symbol("[no LineTerminator here]") -> NoLineTerminator,
      Symbol("<LT>") -> CodePointAbbr("LT"),
      Symbol("> any Unicode code point") -> UnicodeSet(None),
      Symbol("> any Unicode code point with the Unicode property “ID_Start”") ->
        UnicodeSet(Some("with the Unicode property “ID_Start”")),
    )

    checkEquals("NtArg")(
      NtArg("+Await") -> NtArg(NtArg.Kind.True, "Await"),
      NtArg("~Yield") -> NtArg(NtArg.Kind.False, "Yield"),
      NtArg("?Wait") -> NtArg(NtArg.Kind.Pass, "Wait"),
    )

    checkEquals("RhsCond")(
      RhsCond("[+Hello]") -> RhsCond("Hello", true),
      RhsCond("[~Bye]") -> RhsCond("Bye", false),
    )

    val rhsCond: RhsCond = RhsCond("Yield", true)
    val rhs1: Rhs = Rhs(Some(rhsCond), symbols, None)
    val rhs2: Rhs = Rhs(None, symbols, Some("this-is-id"))
    val rhs3: Rhs = Rhs(None, List(Terminal("a")), None)
    val lhs1 = Lhs("Identifier", List("Yield", "Await", "In"))
    val lhs2 = Lhs("Identifier", Nil)
    val prod1 =
      Production(lhs2, Production.Kind.Lexical, true, List(rhs3, rhs3))
    val prod2 =
      Production(lhs2, Production.Kind.Normal, false, List(rhs1, rhs2))
    val prod3 =
      Production(lhs1, Production.Kind.NumericString, false, List(rhs1))

    checkEquals("Rhs")(
      Rhs("[+Yield] `{` `}`") -> rhs1,
      Rhs("`{` `}` #this-is-id") -> rhs2,
      Rhs("`a`") -> rhs3,
    )

    checkEquals("Lhs")(
      Lhs("Identifier[Yield, Await, In]") -> lhs1,
      Lhs("Identifier") -> lhs2,
    )

    checkEquals("Production")(
      Production("""Identifier :: one of
                   |  `a` `a`""".stripMargin) -> prod1,
      Production("""Identifier :
                   |  [+Yield] `{` `}`
                   |  `{` `}` #this-is-id""".stripMargin) -> prod2,
      Production("""Identifier[Yield, Await, In] :::
                   |  [+Yield] `{` `}`""".stripMargin) -> prod3,
    )

    // // /////////////////////////////////////////////////////////////////////////
    // // Algorithm
    // // /////////////////////////////////////////////////////////////////////////
    // // TODO
  }

  init
}
