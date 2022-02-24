package esmeta.spec

import esmeta.util.BaseUtils.*
import esmeta.spec.*
import Symbol.*

class StringifyTinyTest extends SpecTest {
  val name: String = "specStringifyTest"

  // registration
  def init: Unit = {
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
    checkStringify("Grammar")(
      Grammar(List(prod1), List(prod2)) ->
      s"""// Productions
           |Identifier :: one of
           |  `a` `a`
           |
           |// Productions for Web
           |Identifier :
           |  [+Yield] `{` `}`
           |  `{` `}` #this-is-id""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // BuiltinHead Reference
    // -------------------------------------------------------------------------
    import BuiltinHead.Ref.*
    checkStringify("BuiltinHead.Reference")(
      IntrinsicBase("A") -> "%A%",
      NormalBase("A") -> "A",
      NormalAccess(NormalBase("A"), "B") -> "A.B",
      Getter(NormalAccess(NormalBase("A"), "B")) -> "get:A.B",
      Setter(NormalAccess(NormalBase("A"), "B")) -> "set:A.B",
      SymbolAccess(NormalBase("A"), "B") -> "A[@@B]",
      YetRef("A B C") -> "yet:ABC",
    )

    // -------------------------------------------------------------------------
    // Algorithm Head
    // -------------------------------------------------------------------------
    import SyntaxDirectedOperationHead.Target
    checkStringify("Head")(
      AbstractOperationHead(
        "StringIndexOf",
        List(
          Param("string", Param.Kind.Normal, "a String"),
          Param("searchValue", Param.Kind.Normal, "a String"),
          Param("fromIndex", Param.Kind.Normal, "a non-negative integer"),
        ),
        false,
      ) -> "StringIndexOf(string, searchValue, fromIndex)",
      NumericMethodHead(
        "Number",
        "unaryMinus",
        List(
          Param("x", Param.Kind.Normal, "a Number"),
        ),
      ) -> "Number::unaryMinus(x)",
      SyntaxDirectedOperationHead(
        None,
        "VarDeclaredNames",
        true,
        List(Param("withParam", Param.Kind.Normal, "")),
      ) -> "[SYNTAX] <DEFAULT>.VarDeclaredNames[S](withParam)",
      SyntaxDirectedOperationHead(
        Some(
          Target(
            "ForStatement",
            0,
            5,
            List(
              Param("Expression0", Param.Kind.Normal, ""),
              Param("Expression1", Param.Kind.Normal, ""),
              Param("Expression2", Param.Kind.Normal, ""),
              Param("Statement", Param.Kind.Normal, ""),
            ),
          ),
        ),
        "VarDeclaredNames",
        true,
        List(Param("withParam", Param.Kind.Normal, "")),
      ) -> "[SYNTAX] ForStatement[0, 5](Expression0, Expression1, Expression2, Statement).VarDeclaredNames[S](withParam)",
      SyntaxDirectedOperationHead(
        Some(
          Target(
            "AdditiveExpression",
            1,
            0,
            List(
              Param("AdditiveExpression", Param.Kind.Normal, ""),
              Param("MultiplicativeExpression", Param.Kind.Normal, ""),
            ),
          ),
        ),
        "Evaluation",
        false,
        List(),
      ) -> "[SYNTAX] AdditiveExpression[1, 0](AdditiveExpression, MultiplicativeExpression).Evaluation[R]()",
      ConcreteMethodHead(
        "HasBinding",
        Param("envRec", Param.Kind.Normal, "a declarative Environment Record"),
        List(Param("N", Param.Kind.Normal, "a String")),
      ) -> "[METHOD] HasBinding(envRec)(N)", // Old: [METHOD] DeclarativeEnvironmentRecord.HasBinding(envRec)(N)
      InternalMethodHead(
        "SetPrototypeOf",
        Param("O", Param.Kind.Normal, "an ordinary object"),
        List(Param("V", Param.Kind.Normal, "an Object or null")),
      ) -> "[METHOD] SetPrototypeOf(O)(V)", // Old: [METHOD] OrdinaryObject.SetPrototypeOf(O)(V)
      BuiltinHead(
        NormalBase("Boolean"),
        List(Param("value", Param.Kind.Normal, "an argument")),
      ) -> "[BUILTIN] Boolean(value)",
    )
  }

  init
}
