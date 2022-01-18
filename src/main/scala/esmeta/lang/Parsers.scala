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
    rep1(next ~ "1." ~> subStep) ^^ { StepBlock(_) } |
      rep1(next ~ "*" ~> (expr <~ guard(EOL) | yetExpr)) ^^ { ExprBlock(_) } |
      next ~> figureStr ^^ { Figure(_) }
  ) <~ dedent

  // sub-steps
  lazy val subStep: P[SubStep] =
    opt("[id=\"" ~> "[-a-zA-Z0-9]+".r <~ "\"]") ~ (upper ~> step) ^^ {
      case x ~ s =>
        SubStep(x, s)
    }

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
      forEachStep |
      forEachIntStep |
      throwStep |
      performStep |
      appendStep |
      repeatStep |
      pushStep |
      noteStep |
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
    ("if" ~> cond <~ "," ~ opt("then")) ~ step ~
      opt(next ~ "1." ~ ("Else" | "Otherwise") ~ opt(",") ~> step) ^^ {
        case c ~ t ~ e => IfStep(c, t, e)
      }

  // return steps
  lazy val returnStep: P[ReturnStep] =
    "return" ~> expr <~ end ^^ { ReturnStep(_) }

  // assertion steps
  lazy val assertStep: P[AssertStep] =
    "assert" ~ ":" ~> cond <~ end ^^ { AssertStep(_) }

  // for-each steps
  lazy val forEachStep: P[ForEachStep] =
    ("for each" ~> ty) ~ ref ~ ("of" ~> expr) ~ ("," ~ opt("do") ~> step) ^^ {
      case t ~ r ~ e ~ s => ForEachStep(t, r, e, s)
    }

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
    ("perform" | "call") ~> expr <~ end ^^ { PerformStep(_) }

  // append steps
  lazy val appendStep: P[AppendStep] =
    ("append" | "add") ~> expr ~
      ((("to" ~ opt("the end of")) | "as the last element of") ~> ref) <~ end
      ^^ { case e ~ r => AppendStep(e, r) }

  // repeat steps
  lazy val repeatStep: P[RepeatStep] =
    ("repeat" ~ ",") ~> opt(("until" | "while") ~> cond <~ ",") ~ step ^^ {
      case c ~ s => RepeatStep(c, s)
    }

  // push steps
  lazy val pushStep: P[PushStep] =
    "push" ~> ref <~
      ("onto the execution context stack;" ~ ref ~ "is now the running execution context" ~ end) ^^ {
        case r => PushStep(r)
      }

  // note steps
  lazy val noteStep: P[NoteStep] =
    ("NOTE" ~ ":") ~> ".*".r ^^ { str => NoteStep(str) }

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
    recordExpr |||
      typeCheckExpr |||
      lengthExpr |||
      substrExpr |||
      intrExpr |||
      calcExpr |||
      invokeExpr |||
      returnIfAbruptExpr |||
      listExpr

  // record expressions
  lazy val recordExpr: P[RecordExpression] =
    opt("the") ~> rep1(word) ~ ("{" ~> (
      repsep((field <~ ":") ~ expr, ","),
    ) <~ "}") ^^ { case xs ~ fs =>
      var name = xs.mkString(" ")
      if (name endsWith "Record") name = name.dropRight("Record".length).trim
      val fields = fs.map { case f ~ e => f -> e }
      RecordExpression(if (name.isEmpty) None else Some(name), fields)
    }

  // type check expressions
  lazy val typeCheckExpr: P[TypeCheckExpression] =
    ("Type(" ~> expr <~ ")") ~ ("is" ~> opt("not")) ~ ty ^^ { case e ~ n ~ t =>
      TypeCheckExpression(e, t, n.isDefined)
    }

  // `length of` expressions
  lazy val lengthExpr: P[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) }

  // `substring of` expressions
  lazy val substrExpr: P[SubstringExpression] =
    ("the substring of" ~> expr) ~
      ("from" ~> expr) ~
      ("to" ~> expr) ^^ { case e ~ f ~ t => SubstringExpression(e, f, t) }

  // intrinsic expressions
  lazy val intrExpr: P[IntrinsicExpression] = intr ^^ { IntrinsicExpression(_) }

  // reference expressions
  lazy val refExpr: P[ReferenceExpression] =
    ref ^^ { ReferenceExpression(_) }

  // calculation expressions
  lazy val calcExpr: P[CalcExpression] = {
    import MathOpExpression.Op.*
    import BinaryExpression.Op.*
    import UnaryExpression.Op.*

    lazy val base: Parser[CalcExpression] =
      refExpr ||| literal ||| (
        ("-" | "the result of negating") ^^^ Neg
      ) ~ base ^^ { case o ~ e => UnaryExpression(o, e) } ||| (
        "max" ^^^ Max ||| "min" ^^^ Min |||
          "abs" ^^^ Abs ||| "floor" ^^^ Floor |||
          "ℤ" ^^^ ToBigInt ||| "𝔽" ^^^ ToNumber ||| "ℝ" ^^^ ToMath
      ) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case o ~ as =>
        MathOpExpression(o, as)
      }

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
  }

  // literals
  lazy val literal: P[Literal] =
    opt("the") ~> "*this* value" ^^^ ThisLiteral |||
      opt("the") ~ "|" ~> word <~ "|" ^^ { NonterminalLiteral(_) } |||
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
    expr ~ ("has" ~ ("an" | "a") ~> field <~ "internal slot") ^^ { case e ~ f =>
      HasFieldCondition(e, f)
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
    variable ~ rep("." ~> field) ^^ { case x ~ fs =>
      fs.foldLeft[Reference](x)(FieldReference(_, _))
    }

  // variables
  lazy val variable: P[Variable] =
    "_[^_]+_".r ^^ { case s => Variable(s.substring(1, s.length - 1)) }

  // ---------------------------------------------------------------------------
  // algorithm fields
  // ---------------------------------------------------------------------------
  given field: P[Field] =
    "[[" ~> (word ^^ { StringField(_) } | intr ^^ { IntrinsicField(_) }) <~ "]]"

  // ---------------------------------------------------------------------------
  // algorithm intrinsics
  // ---------------------------------------------------------------------------
  given intr: P[Intrinsic] = "%" ~> (word ~ rep("." ~> word)) <~ "%" ^^ {
    case b ~ ps => Intrinsic(b, ps)
  }

  // ---------------------------------------------------------------------------
  // algorithm types
  // ---------------------------------------------------------------------------
  given ty: P[Type] = word ^^ { Type(_) }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // separators
  private def sep(s: Parser[Any]): Parser[Any] = (
    "," ||| "," ~ s ||| s
  )
}
