package esmeta.lang

import esmeta.util.IndentParsers
import scala.util.matching.Regex

/** language parsers */
trait Parsers extends IndentParsers {
  type P[T] = PackratParser[T]

  // ---------------------------------------------------------------------------
  // algorithm blocks
  // ---------------------------------------------------------------------------
  given block: P[Block] = indent ~> (
    rep1(next ~ "1." ~ upper ~> step) ^^ { Order(_) } |
      rep1(next ~ "*" ~> step) ^^ { Unorder(_) } |
      next ~> figureStr ^^ { Figure(_) }
  ) <~ dedent

  lazy val figureStr: P[List[String]] = "<figure>\n".r ~> repsep(
    ".*".r.filter(_.trim != "</figure>"),
    "\n",
  ) <~ "\n *</figure>".r

  // ---------------------------------------------------------------------------
  // algorithm steps
  // ---------------------------------------------------------------------------
  given step: P[Step] = (
    letStep |
      ifStep |
      returnStep,
  ) <~ guard(EOL) | yetStep

  lazy val letStep: P[LetStep] =
    ("let" ~> variable <~ "be") ~ expr <~ "." ^^ { case x ~ e => LetStep(x, e) }

  lazy val ifStep: P[IfStep] =
    ("if" ~> cond <~ ",") ~ step ^^ { case c ~ e => IfStep(c, e, None) }

  lazy val returnStep: P[ReturnStep] =
    "return" ~> expr <~ "." ^^ { ReturnStep(_) }

  lazy val yetStep: Parser[YetStep] =
    ".+".r ~ opt(block) ^^ { case s ~ b => YetStep(s, b) }

  // ---------------------------------------------------------------------------
  // algorithm expressions
  // ---------------------------------------------------------------------------
  given expr: P[Expression] = lengthExpr | idExpr | literal

  lazy val lengthExpr: P[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) }

  lazy val idExpr: P[IdentifierExpression] =
    id ^^ { IdentifierExpression(_) }

  lazy val literal: P[Literal] =
    "the empty String" ^^^ EmptyString

  // ---------------------------------------------------------------------------
  // algorithm conditions
  // ---------------------------------------------------------------------------
  given cond: P[Condition] =
    baseCond ~ rep("and" ~> baseCond) ^^ { case l ~ rs =>
      rs.foldLeft(l)(LogicalAndCondition(_, _))
    }

  lazy val baseCond: P[Condition] =
    expr ~ eqOp ~ expr ^^ { case l ~ o ~ r => EqualCondition(l, o, r) } |
      expr ^^ { ExpressionCondition(_) }

  lazy val eqOp: P[EqualOp] =
    import EqualOp.*
    "is" ^^^ Is |||
      "is not" ^^^ NIs |||
      "=" ^^^ Eq |||
      "≠" ^^^ NEq |||
      "<" ^^^ LessThan |||
      "≤" ^^^ LessThanEqual |||
      ">" ^^^ GreaterThan |||
      "≥" ^^^ GreaterThanEqual

  // ---------------------------------------------------------------------------
  // algorithm identifiers
  // ---------------------------------------------------------------------------
  given id: P[Identifier] = variable

  lazy val variable: P[Variable] =
    "_[^_]*_".r ^^ { case s => Variable(s.substring(1, s.length - 1)) }
}
