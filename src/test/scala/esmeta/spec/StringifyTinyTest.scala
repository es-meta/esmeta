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
        Summary.GrammarElem(145, 16, 195, 28),
        Summary.AlgorithmElem(2258, 365),
        Summary.StepElem(18307, 754),
        Summary.TypeElem(0, 7451),
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
         |- types: 7451 (0.00%)
         |  - known: 0
         |  - unknown: 7451
         |- tables: 89
         |- type model: 58""".stripMargin,
    )
    // -------------------------------------------------------------------------
    // Grammar
    // -------------------------------------------------------------------------
    // pre-defined values
    val ntArgs = List(
      NtArg(NtArg.Kind.True, "Await"),
      NtArg(NtArg.Kind.False, "Yield"),
      NtArg(NtArg.Kind.Pass, "For"),
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
      Production(lhs2, Production.Kind.Lexical, true, List(rhs3, rhs3))
    val prod2 =
      Production(lhs2, Production.Kind.Syntactic, false, List(rhs1, rhs2))
    val prod3 =
      Production(lhs1, Production.Kind.NumericString, false, List(rhs1))

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
    checkStringify("Production.Kind")(
      Production.Kind.Syntactic -> ":",
      Production.Kind.Lexical -> "::",
      Production.Kind.NumericString -> ":::",
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
    import BuiltinHead.Ref.*
    checkStringify("BuiltinHead.Reference")(
      Base("A") -> "A",
      NormalAccess(Base("A"), "B") -> "A.B",
      Getter(NormalAccess(Base("A"), "B")) -> "get A.B",
      Setter(NormalAccess(Base("A"), "B")) -> "set A.B",
      SymbolAccess(Base("A"), "B") -> "A[@@B]",
      YetRef("A B C") -> "yet:ABC",
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
          Param("string", Param.Kind.Normal, Type("a String")),
          Param("searchValue", Param.Kind.Normal, Type("a String")),
          Param(
            "fromIndex",
            Param.Kind.Normal,
            Type("a non-negative integer"),
          ),
        ),
        Type(),
      ) -> "StringIndexOf(string, searchValue, fromIndex): unknown",
      NumericMethodHead(
        Type("Number"),
        "unaryMinus",
        List(
          Param("x", Param.Kind.Normal, Type("a Number")),
        ),
        Type(),
      ) -> "Number::unaryMinus(x): unknown",
      SyntaxDirectedOperationHead(
        None,
        "VarDeclaredNames",
        true,
        List(Param("withParam")),
        Type(),
      ) -> "[SYNTAX] <DEFAULT>.VarDeclaredNames[S](withParam): unknown",
      SyntaxDirectedOperationHead(
        Some(
          Target(
            "ForStatement",
            0,
            5,
            List(
              Param("Expression0"),
              Param("Expression1"),
              Param("Expression2"),
              Param("Statement"),
            ),
          ),
        ),
        "VarDeclaredNames",
        true,
        List(Param("withParam")),
        Type(),
      ) -> "[SYNTAX] ForStatement[0, 5](Expression0, Expression1, Expression2, Statement).VarDeclaredNames[S](withParam): unknown",
      SyntaxDirectedOperationHead(
        Some(
          Target(
            "AdditiveExpression",
            1,
            0,
            List(
              Param("AdditiveExpression"),
              Param("MultiplicativeExpression"),
            ),
          ),
        ),
        "Evaluation",
        false,
        List(),
        Type(),
      ) -> "[SYNTAX] AdditiveExpression[1, 0](AdditiveExpression, MultiplicativeExpression).Evaluation[R](): unknown",
      ConcreteMethodHead(
        "HasBinding",
        Param(
          "envRec",
          Param.Kind.Normal,
          Type("a declarative Environment Record"),
        ),
        List(Param("N", Param.Kind.Normal, Type("a String"))),
        Type(),
      ) -> "[METHOD] HasBinding(envRec)(N): unknown", // Old: [METHOD] DeclarativeEnvironmentRecord.HasBinding(envRec)(N)
      InternalMethodHead(
        "SetPrototypeOf",
        Param("O", Param.Kind.Normal, Type("an ordinary object")),
        List(
          Param("V", Param.Kind.Normal, Type("an Object or null")),
        ),
        Type(),
      ) -> "[METHOD] SetPrototypeOf(O)(V): unknown", // Old: [METHOD] OrdinaryObject.SetPrototypeOf(O)(V)
      BuiltinHead(
        Base("Boolean"),
        List(Param("value", Param.Kind.Normal, Type("an argument"))),
        Type(),
      ) -> "[BUILTIN] Boolean(value): unknown",
    )
  }

  init
}
