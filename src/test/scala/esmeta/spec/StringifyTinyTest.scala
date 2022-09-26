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
    // -------------------------------------------------------------------------
    // Summary
    // -------------------------------------------------------------------------
    checkParseAndStringify("Summary", Summary)(
      Summary(
        Some(Spec.Version("main", "2j3foijwo2")),
        GrammarSummary(145, 16, 195, 28),
        AlgorithmSummary(2258, 365),
        StepSummary(18307, 754),
        TypeSummary(5469, 1543, 439),
        89,
        58,
      ) ->
      s"""- version: main (2j3foijwo2)
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
         |  - unknown: 1543
         |  - notyet: 439
         |- tables: 89
         |- type model: 58""".stripMargin,
    )
    // -------------------------------------------------------------------------
    // Grammar
    // -------------------------------------------------------------------------
    // pre-defined values
    val ntArgs = List(
      NonterminalArgument(NonterminalArgumentKind.True, "Await"),
      NonterminalArgument(NonterminalArgumentKind.False, "Yield"),
      NonterminalArgument(NonterminalArgumentKind.Pass, "For"),
    )
    val nt: Nonterminal = Nonterminal("Identifier", Nil, false)
    val symbols = List(Terminal("{"), Terminal("}"))

    // symbol
    checkParseAndStringify("Symbol", Symbol)(
      Terminal("{") -> "`{`",
      Nonterminal("Identifier", ntArgs, true) ->
      "Identifier[+Await, ~Yield, ?For]?",
      Nonterminal("Identifier", Nil, false) -> "Identifier",
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

    // rhs conditions
    checkParseAndStringify("RhsCond", RhsCond)(
      RhsCond("Hello", true) -> "[+Hello]",
      RhsCond("Bye", false) -> "[~Bye]",
    )

    val rhsCond: RhsCond = RhsCond("Yield", true)
    val rhs1: Rhs = Rhs(Some(rhsCond), symbols, None)
    val rhs2: Rhs = Rhs(None, symbols, Some("this-is-id"))
    val rhs3: Rhs = Rhs(None, List(Terminal("a")), None)
    val lhs1 = Lhs("Identifier", List("Yield", "Await", "In"))
    val lhs2 = Lhs("Identifier", Nil)
    val prod1 =
      Production(lhs2, ProductionKind.Lexical, true, List(rhs3, rhs3))
    val prod2 =
      Production(lhs2, ProductionKind.Syntactic, false, List(rhs1, rhs2))
    val prod3 =
      Production(lhs1, ProductionKind.NumericString, false, List(rhs1))

    // rhs
    checkParseAndStringify("Rhs", Rhs)(
      rhs1 -> "[+Yield] `{` `}`",
      rhs2 -> "`{` `}` #this-is-id",
      rhs3 -> "`a`",
    )

    // lhs
    checkParseAndStringify("Lhs", Lhs)(
      lhs1 -> "Identifier[Yield, Await, In]",
      lhs2 -> "Identifier",
    )

    // production
    checkParseAndStringify("Production", Production)(
      prod1 -> """Identifier :: one of
                 |  `a` `a`""".stripMargin,
      prod2 -> """Identifier :
                 |  [+Yield] `{` `}`
                 |  `{` `}` #this-is-id""".stripMargin,
      prod3 -> """Identifier[Yield, Await, In] :::
                 |  [+Yield] `{` `}`""".stripMargin,
    )

    // production kinds
    checkStringify("ProductionKind")(
      ProductionKind.Syntactic -> ":",
      ProductionKind.Lexical -> "::",
      ProductionKind.NumericString -> ":::",
    )

    // grammar
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
    // BuiltinHead Reference
    // -------------------------------------------------------------------------
    import BuiltinPath.*
    checkStringify("BuiltinPath")(
      Base("A") -> "A",
      NormalAccess(Base("A"), "B") -> "A.B",
      Getter(NormalAccess(Base("A"), "B")) -> "get A.B",
      Setter(NormalAccess(Base("A"), "B")) -> "set A.B",
      SymbolAccess(Base("A"), "B") -> "A[@@B]",
      YetPath("A B C") -> "yet:ABC",
    )

    // -------------------------------------------------------------------------
    // Algorithm Head
    // -------------------------------------------------------------------------
    import SyntaxDirectedOperationHead.Target
    checkStringify("Head")(
      AbstractOperationHead(
        false,
        "StringIndexOf",
        List(
          Param("string", UnknownType("a String")),
          Param("searchValue", UnknownType("a String")),
          Param("fromIndex", UnknownType("a non-negative integer")),
        ),
        UnknownType,
      ) -> "StringIndexOf(string, searchValue, fromIndex): unknown",
      NumericMethodHead(
        UnknownType("Number"),
        "unaryMinus",
        List(
          Param("x", UnknownType("a Number")),
        ),
        UnknownType,
      ) -> "Number::unaryMinus(x): unknown",
      SyntaxDirectedOperationHead(
        None,
        "VarDeclaredNames",
        true,
        List(Param("withParam", UnknownType)),
        UnknownType,
      ) -> "[SYNTAX] <DEFAULT>.VarDeclaredNames[S](withParam): unknown",
      SyntaxDirectedOperationHead(
        Some(
          Target(
            "ForStatement",
            0,
            5,
            List(
              Param("Expression0", UnknownType),
              Param("Expression1", UnknownType),
              Param("Expression2", UnknownType),
              Param("Statement", UnknownType),
            ),
          ),
        ),
        "VarDeclaredNames",
        true,
        List(Param("withParam", UnknownType)),
        UnknownType,
      ) -> "[SYNTAX] ForStatement[0, 5](Expression0, Expression1, Expression2, Statement).VarDeclaredNames[S](withParam): unknown",
      SyntaxDirectedOperationHead(
        Some(
          Target(
            "AdditiveExpression",
            1,
            0,
            List(
              Param("AdditiveExpression", UnknownType),
              Param("MultiplicativeExpression", UnknownType),
            ),
          ),
        ),
        "Evaluation",
        false,
        List(),
        UnknownType,
      ) -> "[SYNTAX] AdditiveExpression[1, 0](AdditiveExpression, MultiplicativeExpression).Evaluation[R](): unknown",
      ConcreteMethodHead(
        "HasBinding",
        Param(
          "envRec",
          UnknownType("a declarative Environment Record"),
        ),
        List(Param("N", UnknownType("a String"))),
        UnknownType,
      ) -> "[METHOD] HasBinding(envRec)(N): unknown", // Old: [METHOD] DeclarativeEnvironmentRecord.HasBinding(envRec)(N)
      InternalMethodHead(
        "SetPrototypeOf",
        Param("O", UnknownType("an ordinary object")),
        List(
          Param("V", UnknownType("an Object or null")),
        ),
        UnknownType,
      ) -> "[METHOD] SetPrototypeOf(O)(V): unknown", // Old: [METHOD] OrdinaryObject.SetPrototypeOf(O)(V)
      BuiltinHead(
        Base("Boolean"),
        List(Param("value", UnknownType("an argument"))),
        UnknownType,
      ) -> "[BUILTIN] Boolean(value): unknown",
    )
  }

  init
}
