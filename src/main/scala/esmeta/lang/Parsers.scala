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
    letStep
  ) <~ guard("\n|$".r) | yetStep

  lazy val letStep: P[LetStep] =
    ("let" ~> variable <~ "be") ~ expr <~ "." ^^ { case x ~ e => LetStep(x, e) }

  lazy val yetStep: Parser[YetStep] =
    ".+".r ~ opt(block) ^^ { case s ~ b => YetStep(s, b) }

  // ---------------------------------------------------------------------------
  // algorithm expressions
  // ---------------------------------------------------------------------------
  given expr: P[Expression] = lengthExpr | idExpr

  lazy val lengthExpr: P[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) }
  lazy val idExpr: P[IdentifierExpression] =
    id ^^ { IdentifierExpression(_) }

  // ---------------------------------------------------------------------------
  // algorithm identifiers
  // ---------------------------------------------------------------------------
  given id: P[Identifier] = variable

  lazy val variable: P[Variable] =
    "_[^_]*_".r ^^ { case s => Variable(s.substring(1, s.length - 1)) }
}
