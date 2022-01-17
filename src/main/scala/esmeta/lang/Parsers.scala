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
      setStep |
      ifStep |
      returnStep |
      assertStep |
      forEachIntStep |
      throwStep |
      performStep |
      blockStep
  ) <~ guard(EOL) | yetStep

  lazy val letStep: P[LetStep] =
    ("let" ~> variable <~ "be") ~ expr <~ end ^^ { case x ~ e => LetStep(x, e) }

  lazy val setStep: P[SetStep] =
    ("set" ~> ref <~ "to") ~ expr <~ end ^^ { case r ~ e => SetStep(r, e) }

  lazy val ifStep: P[IfStep] =
    ("if" ~> cond <~ ",") ~ step ^^ { case c ~ e => IfStep(c, e, None) }

  lazy val returnStep: P[ReturnStep] =
    "return" ~> expr <~ end ^^ { ReturnStep(_) }

  lazy val assertStep: P[AssertStep] =
    "assert" ~ ":" ~> cond <~ end ^^ { AssertStep(_) }

  lazy val forEachIntStep: P[ForEachIntegerStep] =
    ("for each" ~ "(non-negative )?integer".r ~> variable) ~
      ("starting with" ~> expr) ~
      ("such that" ~> cond) ~
      (", in" ~> (
        "ascending" ^^^ true | "descending" ^^^ false
      ) <~ "order," ~ opt("do")) ~
      step ^^ { case x ~ start ~ cond ~ asc ~ body =>
        ForEachIntegerStep(x, start, cond, asc, body)
      }

  lazy val throwStep: P[ThrowStep] =
    "throw a *" ~> word <~ "* exception" <~ end ^^ { ThrowStep(_) }

  lazy val performStep: P[PerformStep] =
    "perform" ~> expr <~ end ^^ { PerformStep(_) }

  lazy val blockStep: P[BlockStep] = block ^^ { BlockStep(_) }

  lazy val yetStep: P[YetStep] =
    opt("[YET]") ~> ".+".r ~ opt(block) ^^ { case s ~ b => YetStep(s, b) }

  // end of step
  lazy val end: Parser[String] =
    "; that is, .*".r | "."

  // ---------------------------------------------------------------------------
  // algorithm expressions
  // ---------------------------------------------------------------------------
  given expr: P[Expression] =
    lengthExpr |||
      substrExpr |||
      emptyStringExpr |||
      calcExpr

  lazy val lengthExpr: P[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) }

  lazy val substrExpr: P[SubstringExpression] =
    ("the substring of" ~> expr) ~
      ("from" ~> expr) ~
      ("to" ~> expr) ^^ { case e ~ f ~ t => SubstringExpression(e, f, t) }

  lazy val calcExpr: P[CalcExpression] =
    import UnaryExpression.Op.*
    import BinaryExpression.Op.*
    lazy val base: Parser[CalcExpression] = refExpr ||| literal ||| (
      ("-" | "the result of negating") ^^^ Neg
    ) ~ base ^^ { case o ~ e => UnaryExpression(o, e) } ||| (
      ("?" ^^^ true | "!" ^^^ false)
    ) ~ calcExpr ^^ { case c ~ e => ReturnIfAbruptExpression(e, c) } ||| (
      word ~ ("(" ~> repsep(calcExpr, ",") <~ ")")
    ) ^^ { case x ~ as => InvokeExpression(x, as) }
    lazy val term: Parser[CalcExpression] = base ~ rep(
      ("×" ^^^ Mul ||| "/" ^^^ Div ||| "modulo" ^^^ Mod) ~ base,
    ) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) => BinaryExpression(l, op, r) }
    }
    lazy val calc: Parser[CalcExpression] = term ~ rep(
      ("+" ^^^ Add ||| "-" ^^^ Sub) ~ term,
    ) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) => BinaryExpression(l, op, r) }
    }
    calc

  lazy val refExpr: P[ReferenceExpression] =
    ref ^^ { ReferenceExpression(_) }

  lazy val emptyStringExpr: P[EmptyStringExpression.type] =
    "the empty String" ^^^ EmptyStringExpression

  lazy val literal: P[Literal] =
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
    baseCond ~ rep(compCondOp ~ baseCond) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) => CompoundCondition(l, op, r) }
    }

  lazy val baseCond: P[Condition] =
    expr ~ binCondOp ~ expr ^^ { case l ~ o ~ r => BinaryCondition(l, o, r) } |
      expr ^^ { ExpressionCondition(_) }

  lazy val binCondOp: P[BinaryCondition.Op] =
    import BinaryCondition.Op.*
    "is" ^^^ Is |||
      "is not" ^^^ NIs |||
      "=" ^^^ Eq |||
      "≠" ^^^ NEq |||
      "<" ^^^ LessThan |||
      "≤" ^^^ LessThanEqual |||
      ">" ^^^ GreaterThan |||
      "≥" ^^^ GreaterThanEqual |||
      "is the same sequence of code units as" ^^^ SameCodeUnits

  lazy val compCondOp: P[CompoundCondition.Op] =
    import CompoundCondition.Op.*
    "and" ^^^ And ||| "or" ^^^ Or

  // ---------------------------------------------------------------------------
  // algorithm references
  // ---------------------------------------------------------------------------
  given ref: P[Reference] =
    variable ~ rep(".[[" ~> word <~ "]]") ^^ { case x ~ fs =>
      fs.foldLeft[Reference](x)(Field(_, _))
    }

  lazy val variable: P[Variable] =
    "_[^_]*_".r ^^ { case s => Variable(s.substring(1, s.length - 1)) }
}
