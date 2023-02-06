package esmeta
package spec

import esmeta.lang.*
import esmeta.spec.*
import esmeta.ty.*

/** test for ECMAScript specification (ECMA-262) */
trait SpecTest extends ESMetaTest {
  def category: String = "spec"
}
object SpecTest {

  // Grammar
  lazy val ntArgs = List(
    NonterminalArgument(NonterminalArgumentKind.True, "Await"),
    NonterminalArgument(NonterminalArgumentKind.False, "Yield"),
    NonterminalArgument(NonterminalArgumentKind.Pass, "For"),
  )
  lazy val nt: Nonterminal = Nonterminal("Identifier", Nil, false)
  lazy val symbols = List(Terminal("{"), Terminal("}"))

  lazy val rhsCond: RhsCond = RhsCond("Yield", true)
  lazy val rhs1: Rhs = Rhs(List(rhsCond), symbols, None)
  lazy val rhs2: Rhs = Rhs(Nil, symbols, Some("this-is-id"))
  lazy val rhs3: Rhs = Rhs(Nil, List(Terminal("a")), None)
  lazy val rhs4: Rhs = Rhs(
    List(RhsCond("Hello", true), RhsCond("Bye", false)),
    List(Terminal("a")),
    None,
  )
  lazy val lhs1 = Lhs("Identifier", List("Yield", "Await", "In"))
  lazy val lhs2 = Lhs("Identifier", Nil)
  lazy val prod1 =
    Production(lhs2, ProductionKind.Lexical, true, List(rhs3, rhs3))
  lazy val prod2 =
    Production(lhs2, ProductionKind.Syntactic, false, List(rhs1, rhs2))
  lazy val prod3 =
    Production(lhs1, ProductionKind.NumericString, false, List(rhs1))

  // algorithms

  // algorithm heads
  lazy val aoHead = AbstractOperationHead(
    false,
    "StringIndexOf",
    List(
      Param("string", Type(StrT)),
      Param("searchValue", Type(StrT)),
      Param("fromIndex", Type(NumberT)),
    ),
    UnknownType,
  )
  lazy val numHead = NumericMethodHead(
    Type(NumberT),
    "unaryMinus",
    List(
      Param("x", Type(NumberT)),
    ),
    UnknownType,
  )
  import SyntaxDirectedOperationHead.Target
  lazy val sdoHead1 = SyntaxDirectedOperationHead(
    None,
    "VarDeclaredNames",
    true,
    List(Param("withParam", UnknownType)),
    UnknownType,
  )
  lazy val sdoHead2 = SyntaxDirectedOperationHead(
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
  )
  lazy val sdoHead3 = SyntaxDirectedOperationHead(
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
  )
  lazy val concMethodHead = ConcreteMethodHead(
    "HasBinding",
    Param(
      "envRec",
      UnknownType,
    ),
    List(Param("N", Type(StrT))),
    UnknownType,
  )
  lazy val methodHead = InternalMethodHead(
    "SetPrototypeOf",
    Param("O", Type(ObjectT)),
    List(
      Param("V", Type(ObjectT || NullT)),
    ),
    UnknownType,
  )
  import BuiltinPath.*
  lazy val builtinHead = BuiltinHead(
    Base("Boolean"),
    List(Param("value", UnknownType)),
    UnknownType,
  )
}
