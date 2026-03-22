package esmeta.lang

import esmeta.lang.*
import esmeta.es.util.dsl.DSLParser
import org.scalatest.funsuite.AnyFunSuite

/** DSL parser test — meta-variable parsing */
class DSLParserTinyTest extends AnyFunSuite {

  test("MetaStep parsing") {
    assert(DSLParser.parseStep("$body:step") == MetaStep("$body", false))
    assert(DSLParser.parseStep("$body:step*") == MetaStep("$body", true))
  }

  test("LetStep with meta-variable") {
    DSLParser.parseStep("let $x:var be $e:expr.") match {
      case LetStep(Variable("$x", _), MetaExpression("$e")) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("SetStep with meta-ref") {
    DSLParser.parseStep("set $r:ref to $e:expr.") match {
      case SetStep(MetaReference("$r"), MetaExpression("$e")) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("AppendStep with meta-expr") {
    DSLParser.parseStep("append $e:expr to $ref:var.") match {
      case AppendStep(MetaExpression("$e"), Variable("$ref", _)) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("ReturnStep with meta-expr") {
    DSLParser.parseStep("return $e:expr.") match {
      case ReturnStep(MetaExpression("$e")) => // ok
      case r                                => fail(s"unexpected: $r")
    }
  }

  test("PerformStep with AO call") {
    DSLParser.parseStep(
      "perform IN__SetDataInsert($ref:var, $elem:expr).",
    ) match {
      case PerformStep(
            InvokeAbstractOperationExpression(
              "IN__SetDataInsert",
              List(
                ReferenceExpression(Variable("$ref", _)),
                MetaExpression("$elem"),
              ),
              _,
            ),
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("MetaExpression") {
    assert(DSLParser.parseExpr("$e:expr") == MetaExpression("$e"))
  }

  test("AO call with meta-expr args") {
    DSLParser.parseExpr(
      "IN__SetDataHas($ref:var, $elem:expr)",
    ) match {
      case InvokeAbstractOperationExpression(
            "IN__SetDataHas",
            List(
              ReferenceExpression(Variable("$ref", _)),
              MetaExpression("$elem"),
            ),
            _,
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("MetaCondition") {
    assert(DSLParser.parseCond("$c:cond") == MetaCondition("$c"))
  }

  test("IsAreCondition with meta-var") {
    DSLParser.parseCond("$x:var is *true*") match {
      case IsAreCondition(
            List(ReferenceExpression(Variable("$x", _))),
            false,
            List(TrueLiteral()),
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("MetaReference") {
    assert(DSLParser.parseRef("$r:ref") == MetaReference("$r"))
  }

  test("Variable meta-var") {
    assert(DSLParser.parseRef("$x:var") == Variable("$x"))
  }

  test("ForEachStep with meta-body") {
    DSLParser.parseStep(
      """for each $elem:var of $ref:var, do
        |  1. $body:step*""".stripMargin,
    ) match {
      case ForEachStep(
            _,
            Variable("$elem", _),
            ReferenceExpression(Variable("$ref", _)),
            true,
            BlockStep(
              StepBlock(List(SubStep(_, MetaStep("$body", true)))),
            ),
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  // -------------------------------------------------------------------------
  // BlockStep / multi-step patterns
  // -------------------------------------------------------------------------

  test("BlockStep with mixed steps and meta-step") {
    // Multi-step blocks are parsed as ForEach/If/Repeat bodies
    DSLParser.parseStep(
      """for each $elem:var of $ref:var, do
        |  1. Let $x:var be $e:expr.
        |  1. $rest:step*""".stripMargin,
    ) match {
      case ForEachStep(
            _,
            Variable("$elem", _),
            ReferenceExpression(Variable("$ref", _)),
            true,
            BlockStep(
              StepBlock(
                List(
                  SubStep(
                    _,
                    LetStep(Variable("$x", _), MetaExpression("$e")),
                  ),
                  SubStep(_, MetaStep("$rest", true)),
                ),
              ),
            ),
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("RepeatStep with three-step body") {
    DSLParser.parseStep(
      """repeat, while $x:var < $y:var,
        |  1. Let $a:var be $e:expr.
        |  1. Set $a:var to $e2:expr.
        |  1. $rest:step*""".stripMargin,
    ) match {
      case RepeatStep(
            RepeatStep.LoopCondition.While(_),
            BlockStep(
              StepBlock(
                List(
                  SubStep(_, LetStep(Variable("$a", _), MetaExpression("$e"))),
                  SubStep(
                    _,
                    SetStep(Variable("$a", _), MetaExpression("$e2")),
                  ),
                  SubStep(_, MetaStep("$rest", true)),
                ),
              ),
            ),
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("IfStep with meta-step body") {
    DSLParser.parseStep(
      """if $x:var is *true*, then
        |  1. $body:step*""".stripMargin,
    ) match {
      case IfStep(
            IsAreCondition(
              List(ReferenceExpression(Variable("$x", _))),
              false,
              List(TrueLiteral()),
            ),
            BlockStep(
              StepBlock(List(SubStep(_, MetaStep("$body", true)))),
            ),
            None,
            _,
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("RepeatStep with meta-body") {
    DSLParser.parseStep(
      """repeat, while $x:var < $y:var,
        |  1. $body:step*""".stripMargin,
    ) match {
      case RepeatStep(
            RepeatStep.LoopCondition.While(
              BinaryCondition(
                ReferenceExpression(Variable("$x", _)),
                BinaryConditionOperator.LessThan,
                ReferenceExpression(Variable("$y", _)),
              ),
            ),
            BlockStep(
              StepBlock(List(SubStep(_, MetaStep("$body", true)))),
            ),
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("SetStep with Access (field) and meta-expr") {
    DSLParser.parseStep("set $elem:var.[[Key]] to $e:expr.") match {
      case SetStep(
            Access(Variable("$elem", _), "Key", _, _),
            MetaExpression("$e"),
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("SetStep with IndexLookup") {
    DSLParser.parseStep("set $base:var[$index:var] to ~empty~.") match {
      case SetStep(
            IndexLookup(
              Variable("$base", _),
              ReferenceExpression(Variable("$index", _)),
            ),
            EnumLiteral("empty"),
          ) => // ok
      case r => fail(s"unexpected: $r")
    }
  }

  test("Regular spec step still works") {
    DSLParser.parseStep("let _x_ be _y_.") match {
      case LetStep(Variable("x", _), ReferenceExpression(Variable("y", _))) =>
      // ok
      case r => fail(s"unexpected: $r")
    }
  }
}
