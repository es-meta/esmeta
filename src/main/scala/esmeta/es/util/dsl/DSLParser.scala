package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.{Parsers, Parser => LangParser}

/** Parser for DSL patterns/templates with meta-variable support.
  *
  * Extends the standard spec-language parser with `$name:type` syntax:
  *   - `$name:step` -> MetaStep("$name", false)
  *   - `$name:step*` -> MetaStep("$name", true)
  *   - `$name:expr` -> MetaExpression("$name")
  *   - `$name:cond` -> MetaCondition("$name")
  *   - `$name:ref` -> MetaReference("$name")
  *   - `$name:var` -> Variable("$name")
  */
object DSLParser extends DSLParsers {

  /** Parse a step with meta-variable support. */
  def parseStep(input: String): Step = parseBy(step)(input)

  /** Parse an expression with meta-variable support. */
  def parseExpr(input: String): Expression = parseBy(expr)(input)

  /** Parse a condition with meta-variable support. */
  def parseCond(input: String): Condition = parseBy(cond)(input)

  /** Parse a reference with meta-variable support. */
  def parseRef(input: String): Reference = parseBy(ref)(input)
}

trait DSLParsers extends Parsers {

  // meta-variable identifier: $name
  lazy val metaIdent = "\\$[a-zA-Z_][a-zA-Z0-9_]*".r

  // ---------------------------------------------------------------------------
  // meta-variable parsers
  // ---------------------------------------------------------------------------

  lazy val metaStep: PL[MetaStep] =
    metaIdent <~ ":step*" ^^ { n => MetaStep(n, true) } |
    metaIdent <~ ":step" ^^ { n => MetaStep(n, false) }

  lazy val metaExpr: PL[MetaExpression] =
    metaIdent <~ ":expr" ^^ { n => MetaExpression(n) }

  lazy val metaCond: PL[MetaCondition] =
    metaIdent <~ ":cond" ^^ { n => MetaCondition(n) }

  lazy val metaRef: PL[MetaReference] =
    metaIdent <~ ":ref" ^^ { n => MetaReference(n) }

  lazy val metaVariable: PL[Variable] =
    metaIdent <~ ":var" ^^ { n => Variable(n) }

  // ---------------------------------------------------------------------------
  // override extension points from Parsers
  // ---------------------------------------------------------------------------

  override def extraStep: PL[Step] = metaStep
  override def extraExpr: PL[Expression] = metaExpr |
    "closure" ~> ("(" ~> repsep(
      variable,
      ",",
    ) <~ ")") ~ ("{" ~> step <~ "}") ^^ {
      case ps ~ body => AbstractClosureExpression(ps, Nil, body)
    }
  override def extraCond: PL[Condition] = metaCond
  override def extraRef: PL[Reference] = metaRef
  override def extraVariable: PL[Variable] = metaVariable
}
