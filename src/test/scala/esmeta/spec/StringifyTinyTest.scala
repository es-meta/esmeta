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
    checkParseAndStringify("Summary", Summary)(
      Summary(
        Some(Spec.Version("d048f32e861c2ed4a26f59a50d392918f26da3ba")),
        GrammarSummary(145, 16, 195, 28),
        AlgorithmSummary(2258, 365),
        StepSummary(18307, 754),
        TypeSummary(5469, 439, 1543),
        89,
        58,
      ) ->
      s"""- version: d048f32e861c2ed4a26f59a50d392918f26da3ba (es2023)
         |- grammar:
         |  - productions: 356
         |    - lexical: 145
         |    - numeric string: 16
         |    - syntactic: 195
         |  - extended productions for web: 28
         |- algorithms: 2623 (86.08%)
         |  - complete: 2258
         |  - incomplete: 365
         |- algorithm steps: 19061 (96.04%)
         |  - complete: 18307
         |  - incomplete: 754
         |- types: 5908 (92.57%)
         |  - known: 5469
         |  - yet: 439
         |  - unknown: 1543
         |- tables: 89
         |- type model: 58""".stripMargin,
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
      aoHead -> "StringIndexOf(string, searchValue, fromIndex): unknown",
      numHead -> "Number::unaryMinus(x): unknown",
      sdoHead1 -> "[SYNTAX] <DEFAULT>.VarDeclaredNames[S](withParam): unknown",
      sdoHead2 -> "[SYNTAX] ForStatement[0, 5](Expression0, Expression1, Expression2, Statement).VarDeclaredNames[S](withParam): unknown",
      sdoHead3 -> "[SYNTAX] AdditiveExpression[1, 0](AdditiveExpression, MultiplicativeExpression).Evaluation[R](): unknown",
      concMethodHead -> "[METHOD] HasBinding(envRec)(N): unknown",
      methodHead -> "[METHOD] SetPrototypeOf(O)(V): unknown",
      builtinHead -> "[BUILTIN] Boolean(value): unknown",
    )

    // built-in algorithm paths
    import BuiltinPath.*
    checkStringify("BuiltinPath")(
      Base("A") -> "A",
      NormalAccess(Base("A"), "B") -> "A.B",
      Getter(NormalAccess(Base("A"), "B")) -> "get A.B",
      Setter(NormalAccess(Base("A"), "B")) -> "set A.B",
      SymbolAccess(Base("A"), "B") -> "A[@@B]",
      YetPath("A B C") -> "yet:ABC",
    )
  }

  init
}
