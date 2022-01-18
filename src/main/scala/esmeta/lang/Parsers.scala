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
    rep1(next ~ "1." ~ upper ~> step) ^^ { StepBlock(_) } |
      rep1(next ~ "*" ~> (expr <~ guard(EOL) | yetExpr)) ^^ { ExprBlock(_) } |
      next ~> figureStr ^^ { Figure(_) }
  ) <~ dedent

  // figure string
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

  // let steps
  lazy val letStep: P[LetStep] =
    ("let" ~> variable <~ "be") ~ expr <~ end ^^ { case x ~ e => LetStep(x, e) }

  // set steps
  lazy val setStep: P[SetStep] =
    ("set" ~> ref <~ "to") ~ expr <~ end ^^ { case r ~ e => SetStep(r, e) }

  // if-then-else steps
  lazy val ifStep: P[IfStep] =
    ("if" ~> cond <~ "," ~ opt("then")) ~ step ^^ { case c ~ e =>
      IfStep(c, e, None)
    }

  // return steps
  lazy val returnStep: P[ReturnStep] =
    "return" ~> expr <~ end ^^ { ReturnStep(_) }

  // assertion steps
  lazy val assertStep: P[AssertStep] =
    "assert" ~ ":" ~> cond <~ end ^^ { AssertStep(_) }

  // for-each steps for integers
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

  // throw steps
  lazy val throwStep: P[ThrowStep] =
    "throw a *" ~> word <~ "* exception" <~ end ^^ { ThrowStep(_) }

  // perform steps
  lazy val performStep: P[PerformStep] =
    "perform" ~> expr <~ end ^^ { PerformStep(_) }

  // block steps
  lazy val blockStep: P[BlockStep] = block ^^ { BlockStep(_) }

  // not yet supported steps
  lazy val yetStep: P[YetStep] = yetExpr ^^ { YetStep(_) }

  // end of step
  lazy val end: Parser[String] = "."

  // ---------------------------------------------------------------------------
  // algorithm expressions
  // ---------------------------------------------------------------------------
  given expr: P[Expression] =
    lengthExpr |||
      substrExpr |||
      calcExpr |||
      invokeExpr |||
      returnIfAbruptExpr |||
      listExpr |||
      ntExpr

  // `length of` expressions
  lazy val lengthExpr: P[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) }

  // `substring of` expressions
  lazy val substrExpr: P[SubstringExpression] =
    ("the substring of" ~> expr) ~
      ("from" ~> expr) ~
      ("to" ~> expr) ^^ { case e ~ f ~ t => SubstringExpression(e, f, t) }

  // reference expressions
  lazy val refExpr: P[ReferenceExpression] =
    ref ^^ { ReferenceExpression(_) }

  // calculation expressions
  lazy val calcExpr: P[CalcExpression] =
    import UnaryExpression.Op.*
    import BinaryExpression.Op.*

    lazy val base: Parser[CalcExpression] =
      refExpr ||| literal ||| (
        ("-" | "the result of negating") ^^^ Neg
      ) ~ base ^^ { case o ~ e => UnaryExpression(o, e) }

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

  // literals
  lazy val literal: P[Literal] =
    opt("the") ~> "*this* value" ^^^ ThisLiteral |||
      "~" ~> "[-+a-zA-Z]+".r <~ "~" ^^ { ConstLiteral(_) } |||
      "the empty String" ^^^ StringLiteral("") |||
      opt("the String") ~ "*" ~> string <~ "*" ^^ { StringLiteral(_) } |||
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

  // algorithm invocation expressions
  lazy val invokeExpr: P[InvokeExpression] =
    invokeAOExpr |||
      invokeSDOExpr

  // abstract operation (AO) invocation expressions
  lazy val invokeAOExpr: P[InvokeAbstractOperationExpression] =
    "[A-Z][a-zA-Z0-9]*".r ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case x ~ as =>
        InvokeAbstractOperationExpression(x, as)
    }

  // syntax-directed operation (SDO) invocation expressions
  lazy val invokeSDOExpr: P[InvokeSyntaxDirectedOperationExpression] =
    lazy val name = (opt("the") ~> word)
    lazy val base = ("of" ~> expr)
    lazy val args = repsep(expr, sep("and"))
    lazy val argsPart = (
      "using" ~> args <~ "as" ~ opt("the") ~ ("arguments" | "argument") |||
        "with" ~ ("arguments" | "argument") ~> args
    )
    name ~ base ~ opt(argsPart) ^^ { case x ~ b ~ as =>
      InvokeSyntaxDirectedOperationExpression(b, x, as.getOrElse(Nil))
    }

  // return-if-abrupt expressions
  lazy val returnIfAbruptExpr: P[ReturnIfAbruptExpression] =
    ("?" ^^^ true | "!" ^^^ false) ~ expr ^^ { case c ~ e =>
      ReturnIfAbruptExpression(e, c)
    }

  // list expressions
  lazy val listExpr: P[ListExpression] =
    "a new empty List" ^^^ ListExpression(Nil) |||
      "«" ~> repsep(expr, ",") <~ "»" ^^ { ListExpression(_) } |||
      "a List whose sole element is" ~> expr ^^ { e => ListExpression(List(e)) }

  // nonterminal expressions
  lazy val ntExpr: P[NonterminalExpression] =
    opt("the") ~ "|" ~> word <~ "|" ^^ { NonterminalExpression(_) }

  // not yet supported expressions
  lazy val yetExpr: P[YetExpression] =
    opt("[YET]") ~> ".+".r ~ opt(block) ^^ { case s ~ b => YetExpression(s, b) }

  // ---------------------------------------------------------------------------
  // algorithm conditions
  // ---------------------------------------------------------------------------
  given cond: P[Condition] =
    import CompoundCondition.Op.*
    lazy val op: P[CompoundCondition.Op] = "and" ^^^ And ||| "or" ^^^ Or
    baseCond ~ rep(op ~ baseCond) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) => CompoundCondition(l, op, r) }
    }

  // base conditions
  lazy val baseCond: P[Condition] =
    exprCond |||
      hasFieldCond |||
      binCond

  // expression conditions
  lazy val exprCond: P[ExpressionCondition] =
    expr ^^ { ExpressionCondition(_) }

  // field includsion conditions
  lazy val hasFieldCond: P[HasFieldCondition] =
    expr ~ ("has" ~ ("an" | "a") ~ "[[" ~> word <~ "]] internal slot") ^^ {
      case e ~ f => HasFieldCondition(e, f)
    }

  // binary conditions
  lazy val binCond: P[BinaryCondition] =
    import BinaryCondition.Op.*
    lazy val op: Parser[BinaryCondition.Op] =
      "is" ^^^ Is |||
        "is not" ^^^ NIs |||
        "=" ^^^ Eq |||
        "≠" ^^^ NEq |||
        "<" ^^^ LessThan |||
        "≤" ^^^ LessThanEqual |||
        ">" ^^^ GreaterThan |||
        "≥" ^^^ GreaterThanEqual |||
        "is the same sequence of code units as" ^^^ SameCodeUnits
    expr ~ op ~ expr ^^ { case l ~ o ~ r => BinaryCondition(l, o, r) }

  // ---------------------------------------------------------------------------
  // algorithm references
  // ---------------------------------------------------------------------------
  given ref: P[Reference] =
    variable ~ rep(".[[" ~> word <~ "]]") ^^ { case x ~ fs =>
      fs.foldLeft[Reference](x)(Field(_, _))
    }

  lazy val variable: P[Variable] =
    "_[^_]+_".r ^^ { case s => Variable(s.substring(1, s.length - 1)) }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // separators
  private def sep(s: Parser[Any]): Parser[Any] = (
    "," ||| "," ~ s ||| s
  )
}
