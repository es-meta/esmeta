package esmeta.es.util

import esmeta.lang.*
import esmeta.spec.Algorithm
import esmeta.lang.util.{UnitWalker => LangUnitWalker, Walker => LangWalker}

import scala.collection.mutable
import scala.annotation.tailrec

import dsl.AstExtensions.*

class DSLPath(dslDir: String) extends OptimizationPath {
  def apply(targets: List[Algorithm]) = {
    println(s"Parsing DSL from $dslDir")

    val isSetDataPredicate: dsl.LangElemPredicate = (elem, context) => {
      elem match {
        case Access(_, "SetData", _, _) => true
        case Variable(v, _) =>
          context.variableTypes.get(v).contains("SetData")
        case _ => false
      }
    }

    val isMapDataPredicate: dsl.LangElemPredicate = (elem, context) => {
      elem match {
        case Access(_, "MapData", _, _) => true
        case Variable(v, _) =>
          context.variableTypes.get(v).contains("MapData")
        case _ => false
      }
    }

    val setDataOperRules: List[dsl.Rule] = List(
      // [OPER] SetData Create (Set)
      dsl.StepRule(
        "[OPER] SetData Create (Set)",
        SetStep(
          Variable("$ref"),
          ListExpression(ListExpressionForm.EmptyList(true, None)),
        ),
        Some(
          SetStep(
            Variable("$ref"),
            InvokeAbstractOperationExpression(
              "IN__SetDataCreate",
              List(),
              HtmlTag.None,
            ),
          ),
        ),
        Map("$ref" -> isSetDataPredicate),
      ),
      // [OPER] SetData Create (Let)
      dsl.StepRule(
        "[OPER] SetData Create (Let)",
        LetStep(
          Variable("$ref"),
          ListExpression(ListExpressionForm.EmptyList(true, None)),
        ),
        Some(
          LetStep(
            Variable("$ref"),
            InvokeAbstractOperationExpression(
              "IN__SetDataCreate",
              List(),
              HtmlTag.None,
            ),
          ),
        ),
        Map("$ref" -> isSetDataPredicate),
      ),
      // [OPER] SetData Insert
      dsl.StepRule(
        "[OPER] SetData Insert",
        AppendStep(
          MetaExpression("$elem"),
          Variable("$ref"),
        ),
        Some(
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__SetDataInsert",
              List(
                ReferenceExpression(Variable("$ref")),
                MetaExpression("$elem"),
              ),
              HtmlTag.None,
            ),
          ),
        ),
        Map("$ref" -> isSetDataPredicate),
      ),
      // [OPER] SetData Copy
      dsl.StepRule(
        "[OPER] SetData Copy",
        LetStep(
          Variable("$var"),
          ListCopyExpression(
            ReferenceExpression(Variable("$ref")),
          ),
        ),
        Some(
          LetStep(
            Variable("$var"),
            InvokeAbstractOperationExpression(
              "IN__SetDataCopy",
              List(ReferenceExpression(Variable("$ref"))),
              HtmlTag.None,
            ),
          ),
        ),
        Map("$ref" -> isSetDataPredicate),
      ),
      // [OPER] SetData Has (expression-level)
      dsl.ExpressionRule(
        "[OPER] SetData Has",
        InvokeAbstractOperationExpression(
          "SetDataHas",
          List(
            ReferenceExpression(Variable("$ref")),
            MetaExpression("$elem"),
          ),
          HtmlTag.None,
        ),
        InvokeAbstractOperationExpression(
          "IN__SetDataHas",
          List(
            ReferenceExpression(Variable("$ref")),
            MetaExpression("$elem"),
          ),
          HtmlTag.None,
        ),
        Map("$ref" -> isSetDataPredicate),
      ),
      // [OPER] SetData Size (expression-level)
      dsl.ExpressionRule(
        "[OPER] SetData Size",
        InvokeAbstractOperationExpression(
          "SetDataSize",
          List(ReferenceExpression(Variable("$ref"))),
          HtmlTag.None,
        ),
        InvokeAbstractOperationExpression(
          "IN__SetDataSize",
          List(ReferenceExpression(Variable("$ref"))),
          HtmlTag.None,
        ),
        Map("$ref" -> isSetDataPredicate),
      ),
    )

    val rules: List[dsl.Rule] = setDataOperRules

    val stats = new dsl.TransformStats()

    val result = targets.map { algo =>
      println(s"[*] Processing ${algo.head.fname}")
      val body = pass(algo.body, rules, stats)
      println("=" * 80)
      println()
      algo.copy(body = body)
    }

    stats.printSummary()
    result
  }

  def pass(
    body: Step,
    rules: List[dsl.Rule],
    stats: dsl.TransformStats,
  ): Step = {
    rules.foldLeft(body) { (curr, rule) =>
      val ctx = dsl.Analyzer.buildContext(curr)
      dsl.Transformer.transformStep(rule, curr, ctx, Some(stats))
    }
  }

  @tailrec
  private def fixpoint(
    body: Step,
    rules: List[dsl.Rule],
    stats: dsl.TransformStats,
  ): Step = {
    val nextBody = pass(body, rules, stats)
    if (nextBody == body) body
    else fixpoint(nextBody, rules, stats)
  }
}
