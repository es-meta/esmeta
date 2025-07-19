package esmeta.spec

import esmeta.lang.*
import esmeta.spec.util.JsonProtocol.given
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*

/** JSON test */
class JsonTinyTest extends SpecTest {
  val name: String = "specJsonTest"

  // registration
  def init: Unit = {
    import SpecTest.*

    // -------------------------------------------------------------------------
    // Grammar
    // -------------------------------------------------------------------------
    // symbols
    checkJson("Symbol")(
      Terminal("{") -> Json.obj("term" -> "{".asJson),
      Nonterminal("Identifier", Nil) -> Json.obj(
        "name" -> "Identifier".asJson,
        "args" -> Json.arr(),
      ),
      Optional(Nonterminal("Identifier", ntArgs)) -> Json.obj(
        "symbol" -> Json.obj(
          "name" -> "Identifier".asJson,
          "args" -> Json.arr(
            Json.obj(
              "kind" -> Json.obj("True" -> Json.obj()),
              "name" -> "Await".asJson,
            ),
            Json.obj(
              "kind" -> Json.obj("False" -> Json.obj()),
              "name" -> "Yield".asJson,
            ),
            Json.obj(
              "kind" -> Json.obj("Pass" -> Json.obj()),
              "name" -> "For".asJson,
            ),
          ),
        ),
      ),
      ButNot(nt, List(nt)) -> Json.obj(
        "base" -> Json.obj(
          "name" -> "Identifier".asJson,
          "args" -> Json.arr(),
        ),
        "notCases" -> Json.arr(
          Json.obj(
            "name" -> "Identifier".asJson,
            "args" -> Json.arr(),
          ),
        ),
      ),
      ButOnlyIf(nt, "MV", "> 0x10FFFF") -> Json.obj(
        "base" -> Json.obj(
          "name" -> "Identifier".asJson,
          "args" -> Json.arr(),
        ),
        "methodName" -> "MV".asJson,
        "cond" -> "> 0x10FFFF".asJson,
      ),
      Lookahead(true, List(symbols, symbols)) -> Json.obj(
        "contains" -> Json.True,
        "cases" -> Json.arr(
          Json.arr(
            Json.obj("term" -> "{".asJson),
            Json.obj("term" -> "}".asJson),
          ),
          Json.arr(
            Json.obj("term" -> "{".asJson),
            Json.obj("term" -> "}".asJson),
          ),
        ),
      ),
      Lookahead(false, List(symbols, symbols)) -> Json.obj(
        "contains" -> Json.False,
        "cases" -> Json.arr(
          Json.arr(
            Json.obj("term" -> "{".asJson),
            Json.obj("term" -> "}".asJson),
          ),
          Json.arr(
            Json.obj("term" -> "{".asJson),
            Json.obj("term" -> "}".asJson),
          ),
        ),
      ),
      Empty -> Json.obj("empty" -> Json.Null),
      NoLineTerminator -> Json.obj("nlt" -> Json.Null),
      CodePointAbbr("LT") -> Json.obj("abbr" -> "LT".asJson),
      UnicodeSet(None) -> Json.obj("cpCond" -> Json.Null),
      UnicodeSet(Some("with the Unicode property “ID_Start”")) -> Json.obj(
        "cpCond" -> "with the Unicode property “ID_Start”".asJson,
      ),
    )

    // RHS conditions
    checkJson("RhsCond")(
      RhsCond("Hello", true) -> Json.obj(
        "name" -> "Hello".asJson,
        "pass" -> Json.True,
      ),
      RhsCond("Bye", false) -> Json.obj(
        "name" -> "Bye".asJson,
        "pass" -> Json.False,
      ),
    )

    // RHSs
    lazy val rhsJson1 = Json.obj(
      "conditions" -> Json.arr(
        Json.obj(
          "name" -> "Yield".asJson,
          "pass" -> Json.True,
        ),
      ),
      "symbols" -> Json.arr(
        Json.obj("term" -> "{".asJson),
        Json.obj("term" -> "}".asJson),
      ),
      "id" -> Json.Null,
    )
    lazy val rhsJson2 = Json.obj(
      "conditions" -> Json.arr(
      ),
      "symbols" -> Json.arr(
        Json.obj("term" -> "{".asJson),
        Json.obj("term" -> "}".asJson),
      ),
      "id" -> "this-is-id".asJson,
    )
    lazy val rhsJson3 = Json.obj(
      "conditions" -> Json.arr(),
      "symbols" -> Json.arr(Json.obj("term" -> "a".asJson)),
      "id" -> Json.Null,
    )
    lazy val rhsJson4 = Json.obj(
      "conditions" -> Json.arr(
        Json.obj(
          "name" -> "Hello".asJson,
          "pass" -> Json.True,
        ),
        Json.obj(
          "name" -> "Bye".asJson,
          "pass" -> Json.False,
        ),
      ),
      "symbols" -> Json.arr(Json.obj("term" -> "a".asJson)),
      "id" -> Json.Null,
    )
    checkJson("Rhs")(
      rhs1 -> rhsJson1,
      rhs2 -> rhsJson2,
      rhs3 -> rhsJson3,
      rhs4 -> rhsJson4,
    )

    // LHSs
    lazy val lhsJson1 = Json.obj(
      "name" -> "Identifier".asJson,
      "params" -> Json.arr("Yield".asJson, "Await".asJson, "In".asJson),
    )
    lazy val lhsJson2 = Json.obj(
      "name" -> "Identifier".asJson,
      "params" -> Json.arr(),
    )
    checkJson("Lhs")(
      lhs1 -> lhsJson1,
      lhs2 -> lhsJson2,
    )

    // production kinds
    checkJson("ProductionKind")(
      ProductionKind.Syntactic -> Json.obj("Syntactic" -> Json.obj()),
      ProductionKind.Lexical -> Json.obj("Lexical" -> Json.obj()),
      ProductionKind.NumericString -> Json.obj("NumericString" -> Json.obj()),
    )

    // productions
    lazy val prodJson1 = Json.obj(
      "lhs" -> lhsJson2,
      "kind" -> Json.obj("Lexical" -> Json.obj()),
      "oneof" -> Json.True,
      "rhsVec" -> Json.arr(rhsJson3, rhsJson3),
    )
    lazy val prodJson2 = Json.obj(
      "lhs" -> lhsJson2,
      "kind" -> Json.obj("Syntactic" -> Json.obj()),
      "oneof" -> Json.False,
      "rhsVec" -> Json.arr(rhsJson1, rhsJson2),
    )
    lazy val prodJson3 = Json.obj(
      "lhs" -> lhsJson1,
      "kind" -> Json.obj("NumericString" -> Json.obj()),
      "oneof" -> Json.False,
      "rhsVec" -> Json.arr(rhsJson1),
    )
    checkJson("Production")(
      prod1 -> prodJson1,
      prod2 -> prodJson2,
      prod3 -> prodJson3,
    )

    // grammars
    checkJson("Grammar")(
      Grammar(List(prod1), List(prod2)) -> Json.obj(
        "prods" -> Json.arr(prodJson1),
        "prodsForWeb" -> Json.arr(prodJson2),
      ),
    )

    // -------------------------------------------------------------------------
    // Algorithm Head
    // -------------------------------------------------------------------------
    checkJson("Head")(
      aoHead -> Json.obj(
        "AbstractOperationHead" -> Json.obj(
          "isHostDefined" -> Json.False,
          "name" -> "StringIndexOf".asJson,
          "params" -> Json.arr(
            Json.obj(
              "name" -> "string".asJson,
              "ty" -> "a String".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
            Json.obj(
              "name" -> "searchValue".asJson,
              "ty" -> "a String".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
            Json.obj(
              "name" -> "fromIndex".asJson,
              "ty" -> "a Number".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
          ),
          "retTy" -> "unknown".asJson,
        ),
      ),
      numHead -> Json.obj(
        "NumericMethodHead" -> Json.obj(
          "baseTy" -> "a Number".asJson,
          "name" -> "unaryMinus".asJson,
          "params" -> Json.arr(
            Json.obj(
              "name" -> "x".asJson,
              "ty" -> "a Number".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
          ),
          "retTy" -> "unknown".asJson,
        ),
      ),
      sdoHead1 -> Json.obj(
        "SyntaxDirectedOperationHead" -> Json.obj(
          "target" -> Json.Null,
          "methodName" -> "VarDeclaredNames".asJson,
          "isStatic" -> Json.True,
          "withParams" -> Json.arr(
            Json.obj(
              "name" -> "withParam".asJson,
              "ty" -> "unknown".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
          ),
          "retTy" -> "unknown".asJson,
        ),
      ),
      sdoHead2 -> Json.obj(
        "SyntaxDirectedOperationHead" -> Json.obj(
          "target" -> Json.obj(
            "lhsName" -> "ForStatement".asJson,
            "idx" -> 0.asJson,
            "subIdx" -> 5.asJson,
          ),
          "methodName" -> "VarDeclaredNames".asJson,
          "isStatic" -> Json.True,
          "withParams" -> Json.arr(
            Json.obj(
              "name" -> "withParam".asJson,
              "ty" -> "unknown".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
          ),
          "retTy" -> "unknown".asJson,
        ),
      ),
      sdoHead3 -> Json.obj(
        "SyntaxDirectedOperationHead" -> Json.obj(
          "target" -> Json.obj(
            "lhsName" -> "AdditiveExpression".asJson,
            "idx" -> 1.asJson,
            "subIdx" -> 0.asJson,
          ),
          "methodName" -> "Evaluation".asJson,
          "isStatic" -> Json.False,
          "withParams" -> Json.arr(),
          "retTy" -> "unknown".asJson,
        ),
      ),
      concMethodHead -> Json.obj(
        "ConcreteMethodHead" -> Json.obj(
          "concMethodName" -> "HasBinding".asJson,
          "receiver" -> Json.obj(
            "name" -> "envRec".asJson,
            "ty" -> "unknown".asJson,
            "kind" -> Json.obj("Normal" -> Json.obj()),
          ),
          "params" -> Json.arr(
            Json.obj(
              "name" -> "N".asJson,
              "ty" -> "a String".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
          ),
          "retTy" -> "unknown".asJson,
        ),
      ),
      methodHead -> Json.obj(
        "InternalMethodHead" -> Json.obj(
          "methodName" -> "SetPrototypeOf".asJson,
          "receiver" -> Json.obj(
            "name" -> "O".asJson,
            "ty" -> "unknown".asJson,
            "kind" -> Json.obj("Normal" -> Json.obj()),
          ),
          "params" -> Json.arr(
            Json.obj(
              "name" -> "V".asJson,
              "ty" -> "an Object or *null*".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
          ),
          "retTy" -> "unknown".asJson,
        ),
      ),
      builtinHead -> Json.obj(
        "BuiltinHead" -> Json.obj(
          "path" -> Json.obj(
            "Base" -> Json.obj(
              "name" -> "Boolean".asJson,
            ),
          ),
          "params" -> Json.arr(
            Json.obj(
              "name" -> "value".asJson,
              "ty" -> "unknown".asJson,
              "kind" -> Json.obj("Normal" -> Json.obj()),
            ),
          ),
          "retTy" -> "unknown".asJson,
        ),
      ),
    )

    // built-in algorithm paths
    import BuiltinPath.*
    checkJson("BuiltinPath")(
      Base("A") -> Json.obj("Base" -> Json.obj("name" -> "A".asJson)),
      NormalAccess(Base("A"), "B") -> Json.obj(
        "NormalAccess" -> Json.obj(
          "base" -> Json.obj("Base" -> Json.obj("name" -> "A".asJson)),
          "name" -> "B".asJson,
        ),
      ),
      Getter(NormalAccess(Base("A"), "B")) -> Json.obj(
        "Getter" -> Json.obj(
          "base" -> Json.obj(
            "NormalAccess" -> Json.obj(
              "base" -> Json.obj("Base" -> Json.obj("name" -> "A".asJson)),
              "name" -> "B".asJson,
            ),
          ),
        ),
      ),
      Setter(NormalAccess(Base("A"), "B")) -> Json.obj(
        "Setter" -> Json.obj(
          "base" -> Json.obj(
            "NormalAccess" -> Json.obj(
              "base" -> Json.obj("Base" -> Json.obj("name" -> "A".asJson)),
              "name" -> "B".asJson,
            ),
          ),
        ),
      ),
      SymbolAccess(Base("A"), "B") -> Json.obj(
        "SymbolAccess" -> Json.obj(
          "base" -> Json.obj("Base" -> Json.obj("name" -> "A".asJson)),
          "symbol" -> "B".asJson,
        ),
      ),
      YetPath("A B C") -> Json.obj(
        "YetPath" -> Json.obj("name" -> "A B C".asJson),
      ),
    )
  }

  init
}
