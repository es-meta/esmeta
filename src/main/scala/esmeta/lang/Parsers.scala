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
      returnStep |
      assertStep,
  ) <~ guard(EOL) | yetStep

  lazy val letStep: P[LetStep] =
    ("let" ~> variable <~ "be") ~ expr <~ "." ^^ { case x ~ e => LetStep(x, e) }

  lazy val ifStep: P[IfStep] =
    ("if" ~> cond <~ ",") ~ step ^^ { case c ~ e => IfStep(c, e, None) }

  lazy val returnStep: P[ReturnStep] =
    "return" ~> expr <~ "." ^^ { ReturnStep(_) }

  lazy val assertStep: P[AssertStep] =
    "assert" ~ ":" ~> cond <~ "." ^^ { AssertStep(_) }

  lazy val yetStep: Parser[YetStep] =
    opt("[YET]") ~> ".+".r ~ opt(block) ^^ { case s ~ b => YetStep(s, b) }

  // ---------------------------------------------------------------------------
  // algorithm expressions
  // ---------------------------------------------------------------------------
  given expr: P[Expression] = lengthExpr | idExpr | literal

  lazy val lengthExpr: P[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) }

  lazy val idExpr: P[IdentifierExpression] =
    id ^^ { IdentifierExpression(_) }

  lazy val literal: P[Literal] =
    "the empty String" ^^^ EmptyStringLiteral |||
      "*" ~> string <~ "*" ^^ { StringLiteral(_) } |||
      "+∞" ^^^ PositiveInfinityMathValueLiteral |||
      "-∞" ^^^ NegativeInfinityMathValueLiteral |||
      number ^^ { case s => DecimalMathValueLiteral(BigDecimal(s)) } |||
      "*+∞*<sub>𝔽</sub>" ^^^ NumberLiteral(Double.PositiveInfinity) |||
      "*-∞*<sub>𝔽</sub>" ^^^ NumberLiteral(Double.NegativeInfinity) |||
      "*NaN*" ^^^ NumberLiteral(Double.NaN) |||
      "*" ~> double <~ "*<sub>𝔽</sub>" ^^ { NumberLiteral(_) } |||
      "*" ~> bigint <~ "*<sub>ℤ</sub>" ^^ { BigIntLiteral(_) } |||
      "*true*" ^^^ TrueLiteral |||
      "*false*" ^^^ FalseLiteral |||
      "*undefined*" ^^^ UndefinedLiteral |||
      "*null*" ^^^ NullLiteral

  // ---------------------------------------------------------------------------
  // algorithm conditions
  // ---------------------------------------------------------------------------
  given cond: P[Condition] =
    baseCond ~ rep(cop ~ baseCond) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) => CompoundCondition(l, op, r) }
    }

  lazy val baseCond: P[Condition] =
    expr ~ bop ~ expr ^^ { case l ~ o ~ r => BinaryCondition(l, o, r) } |
      expr ^^ { ExpressionCondition(_) }

  lazy val bop: P[BinaryOp] =
    import BinaryOp.*
    "is" ^^^ Is |||
      "is not" ^^^ NIs |||
      "=" ^^^ Eq |||
      "≠" ^^^ NEq |||
      "<" ^^^ LessThan |||
      "≤" ^^^ LessThanEqual |||
      ">" ^^^ GreaterThan |||
      "≥" ^^^ GreaterThanEqual

  lazy val cop: P[CompoundOp] =
    import CompoundOp.*
    "and" ^^^ And ||| "or" ^^^ Or

  // ---------------------------------------------------------------------------
  // algorithm identifiers
  // ---------------------------------------------------------------------------
  given id: P[Identifier] = variable

  lazy val variable: P[Variable] =
    "_[^_]*_".r ^^ { case s => Variable(s.substring(1, s.length - 1)) }
}
