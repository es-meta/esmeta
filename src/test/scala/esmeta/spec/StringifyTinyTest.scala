package esmeta.spec

import esmeta.lang.*
import esmeta.ty.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*
import Symbol.*

/** stringify test */
class StringifyTinyTest extends SpecTest {
  val name: String = "specStringifyTest"

  // registration
  def init: Unit = {
    import SpecTest.*

    // -------------------------------------------------------------------------
    // Summary
    // -------------------------------------------------------------------------
    val version =
      Spec.Version("d048f32e861c2ed4a26f59a50d392918f26da3ba", Some("es2023"))
    checkParseAndStringify("Summary", Summary)(
      Summary(
        Some(version),
        GrammarSummary(145, 16, 195, 28),
        AlgorithmSummary(2258, 365),
        StepSummary(18307, 754),
        TypeSummary(5469, 439, 1543),
        89,
        58,
        101,
      ) ->
      s"""- version: d048f32e861c2ed4a26f59a50d392918f26da3ba (es2023)
         |- grammar:
         |  - productions: 356
         |    - lexical: 145
         |    - numeric string: 16
         |    - syntactic: 195
         |  - extended productions for web: 28
         |- algorithms: 2258
         |  - complete: 365 (16.16%)
         |  - equals: 0 (0.00%)
         |- algorithm steps: 18307
         |  - complete: 754 (4.12%)
         |- types: 5469
         |  - known: 439 (8.03%)
         |  - yet: 1543 (28.21%)
         |- tables: 89
         |- type model: 58
         |- intrinsics: 101""".stripMargin,
    )
    // -------------------------------------------------------------------------
    // Grammar
    // -------------------------------------------------------------------------
    // symbols
    checkParseAndStringify("Symbol", Symbol)(
      Terminal("{") -> "`{`",
      Nonterminal("Identifier", Nil) -> "Identifier",
      Nonterminal("Identifier", ntArgs) -> "Identifier[+Await, ~Yield, ?For]",
      Optional(Terminal("{")) -> "`{`?",
      Optional(Nonterminal("Identifier", Nil)) -> "Identifier?",
      ButNot(nt, List(nt)) -> "Identifier but not Identifier",
      ButOnlyIf(nt, "MV", "> 0x10FFFF") ->
      "Identifier [> but only if MV of |Identifier|> 0x10FFFF]",
      Lookahead(true, List(symbols, symbols)) ->
      "[lookahead < {`{` `}`, `{` `}`}]",
      Lookahead(false, List(symbols, symbols)) ->
      "[lookahead <! {`{` `}`, `{` `}`}]",
      Empty -> "[empty]",
      NoLineTerminator -> "[no LineTerminator here]",
      CodePointAbbr("LT") -> "<LT>",
      UnicodeSet(None) -> "> any Unicode code point",
      UnicodeSet(Some("with the Unicode property “ID_Start”")) ->
      "> any Unicode code point with the Unicode property “ID_Start”",
    )

    // RHS conditions
    checkParseAndStringify("RhsCond", RhsCond)(
      RhsCond("Hello", true) -> "+Hello",
      RhsCond("Bye", false) -> "~Bye",
    )

    // RHSs
    checkParseAndStringify("Rhs", Rhs)(
      rhs1 -> "[+Yield] `{` `}`",
      rhs2 -> "`{` `}` #this-is-id",
      rhs3 -> "`a`",
      rhs4 -> "[+Hello, ~Bye] `a`",
    )

    // LHSs
    checkParseAndStringify("Lhs", Lhs)(
      lhs1 -> "Identifier[Yield, Await, In]",
      lhs2 -> "Identifier",
    )

    // production kinds
    checkStringify("ProductionKind")(
      ProductionKind.Syntactic -> ":",
      ProductionKind.Lexical -> "::",
      ProductionKind.NumericString -> ":::",
    )

    // productions
    checkParseAndStringify("Production", Production)(
      prod1 -> """Identifier :: one of
                 |  `a` `a`""".stripMargin,
      prod2 -> """Identifier :
                 |  [+Yield] `{` `}`
                 |  `{` `}` #this-is-id""".stripMargin,
      prod3 -> """Identifier[Yield, Await, In] :::
                 |  [+Yield] `{` `}`""".stripMargin,
    )

    // grammars
    checkParseAndStringify("Grammar", Grammar)(
      Grammar(List(prod1), List(prod2)) ->
      s"""<Productions>
         |Identifier :: one of
         |  `a` `a`
         |
         |<Productions for Web>
         |Identifier :
         |  [+Yield] `{` `}`
         |  `{` `}` #this-is-id""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // Algorithm Head
    // -------------------------------------------------------------------------
    checkStringify("Head")(
      aoHead -> """[abstract operation] StringIndexOf(
      |  _string_: a String,
      |  _searchValue_: a String,
      |  _fromIndex_: a Number,
      |): unknown""".stripMargin,
      numHead -> """[numeric method] Number::unaryMinus(
      |  _x_: a Number,
      |): unknown""".stripMargin,
      sdoHead1 -> """[sdo] (static) DEFAULT:VarDeclaredNames(
      |  _withParam_,
      |): unknown""".stripMargin,
      sdoHead2 -> """[sdo] (static) ForStatement[0, 5].VarDeclaredNames(
      |  _withParam_,
      |): unknown""".stripMargin,
      sdoHead3 -> "[sdo] (runtime) AdditiveExpression[1, 0].Evaluation(): unknown",
      concMethodHead -> """[concrete method] HasBinding(_envRec_)(
      |  _N_: a String,
      |): unknown""".stripMargin,
      methodHead -> """[internal method] SetPrototypeOf(_O_)(
      |  _V_: an Object or *null*,
      |): unknown""".stripMargin,
      builtinHead -> """[builtin] Boolean(
      |  _value_,
      |): unknown""".stripMargin,
    )

    // built-in algorithm paths
    import BuiltinPath.*
    checkStringify("BuiltinPath")(
      Base("A") -> "A",
      NormalAccess(Base("A"), "B") -> "A.B",
      Getter(NormalAccess(Base("A"), "B")) -> "get:A.B",
      Setter(NormalAccess(Base("A"), "B")) -> "set:A.B",
      SymbolAccess(Base("A"), "B") -> "A[%Symbol.B%]",
      YetPath("A B C") -> "yet:ABC",
    )
  }

  init
}
