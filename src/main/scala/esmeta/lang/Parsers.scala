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
    rep1(subStep) ^^ { StepBlock(_) } |
      rep1(next ~ "*" ~> (expr <~ guard(EOL) | yetExpr)) ^^ { ExprBlock(_) } |
      next ~> figureStr ^^ { Figure(_) }
  ) <~ dedent

  // sub-steps
  lazy val subStepPrefix: Parser[Option[String]] =
    next ~> "1." ~> opt("[id=\"" ~> "[-a-zA-Z0-9]+".r <~ "\"]") <~ upper
  lazy val subStep: Parser[SubStep] =
    subStepPrefix ~ (step <~ guard(EOL) | yetStep) ^^ { case x ~ s =>
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
  given step: P[Step] =
    letStep |
      setStep |
      returnStep |
      assertStep |
      throwStep |
      performStep |
      appendStep |
      repeatStep |
      pushStep |
      noteStep |
      ifStep |
      forEachStep |
      forEachIntStep |
      blockStep

  // let steps
  lazy val letStep: P[LetStep] =
    ("let" ~> variable <~ "be") ~ expr <~ end ^^ { case x ~ e => LetStep(x, e) }

  // set steps
  lazy val setStep: P[SetStep] =
    ("set" ~> ref <~ "to") ~ expr <~ end ^^ { case r ~ e => SetStep(r, e) }

  // if-then-else steps
  lazy val ifStep: P[IfStep] =
    ("if" ~> cond <~ "," ~ opt("then")) ~ step ~ opt(
      opt(subStepPrefix) ~ ("else" | "otherwise") ~ opt(",") ~> step,
    ) ^^ { case c ~ t ~ e => IfStep(c, t, e) }

  // return steps
  lazy val returnStep: P[ReturnStep] =
    "return" ~> opt(expr) <~ end ^^ { ReturnStep(_) }

  // assertion steps
  lazy val assertStep: P[AssertStep] =
    "assert" ~ ":" ~> cond <~ end ^^ { AssertStep(_) }

  // for-each steps
  lazy val forEachStep: P[ForEachStep] =
    ("for each" ~ opt("element") ~> opt(ty)) ~
      variable ~
      ("of" ~> expr) ~
      ("," ~ opt("do") ~> step) ^^ { case t ~ r ~ e ~ s =>
        ForEachStep(t, r, e, s)
      }

  // for-each steps for integers
  lazy val forEachIntStep: P[ForEachIntegerStep] =
    lazy val ascending: Parser[Boolean] =
      ("ascending" ^^^ true | "descending" ^^^ false)
    ("for each" ~ "(non-negative )?integer".r ~> variable) ~
      ("starting with" ~> expr) ~
      ("such that" ~> cond) ~
      (", in" ~> ascending <~ "order,") ~
      (opt("do") ~> step) ^^ { case x ~ start ~ cond ~ asc ~ body =>
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
  lazy val end: Parser[String] = "." <~ upper | ";"

  // ---------------------------------------------------------------------------
  // algorithm expressions
  // ---------------------------------------------------------------------------
  given expr: P[Expression] =
    stringConcatExpr |||
      recordExpr |||
      typeCheckExpr |||
      lengthExpr |||
      substrExpr |||
      intrExpr |||
      calcExpr |||
      invokeExpr |||
      returnIfAbruptExpr |||
      listExpr

  // string concatenation expressions
  lazy val stringConcatExpr: P[StringConcatExpression] =
    "the string-concatenation of" ~> repsep(expr, sep("and")) ^^ {
      StringConcatExpression(_)
    }

  // record expressions
  lazy val recordExpr: P[RecordExpression] =
    opt("the") ~> ty ~ ("{" ~> repsep((field <~ ":") ~ expr, ",") <~ "}") ^^ {
      case t ~ fs =>
        val fields = fs.map { case f ~ e => f -> e }
        RecordExpression(t, fields)
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

  // calculation expressions
  lazy val calcExpr: P[CalcExpression] = {
    import BinaryExpression.Op.*
    import UnaryExpression.Op.*

    lazy val base: P[CalcExpression] =
      refExpr ||| literal ||| mathOpExpr ||| "(" ~> calc <~ ")" ||| (
        base ~ ("<sup>" ~> calc <~ "</sup>")
      ) ^^ { case b ~ e => ExponentiationExpression(b, e) }

    lazy val unary: P[CalcExpression] = base ||| (
      ("-" | "the result of negating") ^^^ Neg
    ) ~ base ^^ { case o ~ e =>
      UnaryExpression(o, e)
    }

    lazy val term: P[CalcExpression] = unary ~ rep(
      ("×" ^^^ Mul ||| "/" ^^^ Div ||| "modulo" ^^^ Mod) ~ unary,
    ) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) => BinaryExpression(l, op, r) }
    }

    lazy val calc: P[CalcExpression] = term ~ rep(
      ("+" ^^^ Add ||| "-" ^^^ Sub) ~ term,
    ) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) => BinaryExpression(l, op, r) }
    }

    calc
  }

  // reference expressions
  lazy val refExpr: P[ReferenceExpression] =
    ref ^^ { ReferenceExpression(_) }

  // mathematical operation expressions
  lazy val mathOpExpr: P[MathOpExpression] =
    import MathOpExpression.Op.*
    (
      "max" ^^^ Max ||| "min" ^^^ Min |||
        "abs" ^^^ Abs ||| "floor" ^^^ Floor |||
        "ℤ" ^^^ ToBigInt ||| "𝔽" ^^^ ToNumber ||| "ℝ" ^^^ ToMath
    ) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case o ~ as =>
      MathOpExpression(o, as)
    }

  // literals
  lazy val literal: P[Literal] =
    opt("the") ~> "*this* value" ^^^ ThisLiteral |||
      "`[^`]+`".r ^^ { case s => CodeLiteral(s.substring(1, s.length - 1)) } |||
      opt("the") ~ "|" ~> word <~ "|" ^^ { NonterminalLiteral(_) } |||
      "~" ~> "[-+a-zA-Z]+".r <~ "~" ^^ { ConstLiteral(_) } |||
      "the empty String" ^^^ StringLiteral("") |||
      strLiteral |||
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

  lazy val strLiteral: P[StringLiteral] =
    opt("the String") ~> """\*"[^"]*"\*""".r ^^ { case s =>
      val str = s
        .substring(2, s.length - 2)
        .replace("\\*", "*")
        .replace("\\\\", "\\")
      StringLiteral(str)
    }

  // algorithm invocation expressions
  lazy val invokeExpr: P[InvokeExpression] =
    invokeAOExpr |||
      invokeSDOExpr

  // abstract operation (AO) invocation expressions
  lazy val invokeAOExpr: P[InvokeAbstractOperationExpression] =
    "(this)?[A-Z][a-zA-Z0-9]*".r ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
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
      instanceOfCond |||
      hasFieldCond |||
      binCond

  // expression conditions
  lazy val exprCond: P[ExpressionCondition] =
    expr ^^ { ExpressionCondition(_) }

  // instance check conditions
  lazy val instanceOfCond: P[InstanceOfCondition] =
    expr ~ ("is" ~ ("a" | "an") ~> ty) ^^ { case e ~ t =>
      InstanceOfCondition(e, t)
    }

  // field includsion conditions
  lazy val hasFieldCond: P[HasFieldCondition] =
    expr ~ ("has" ~ ("an" | "a") ~> field <~ "internal" ~ ("method" | "slot")) ^^ {
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
  given ty: P[Type] = rep1(camel) ^^ { case ss => Type(ss.mkString(" ")) }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // separators
  private def sep(s: Parser[Any]): Parser[Any] = (
    "," ||| "," ~ s ||| s
  )
}
