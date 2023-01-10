package esmeta.lang.util

import esmeta.lang.*
import esmeta.ty.*
import esmeta.ty.util.{Parsers => TyParsers}
import esmeta.util.{IndentParsers, Locational, ConcreteLattice}
import esmeta.util.BaseUtils.*

/** metalanguage parser */
object Parser extends Parsers
object ParserForEval extends Parsers { override def eval = true }
trait Parsers extends IndentParsers {
  // shortcuts
  type P[T] = EPackratParser[T]
  type PL[T <: Locational] = LocationalParser[T]

  // ---------------------------------------------------------------------------
  // metalanguage blocks
  // ---------------------------------------------------------------------------
  given block: PL[Block] = {
    indent ~> (
      rep1(subStep) ^^ { StepBlock(_) } |
      rep1(next ~ "*" ~> (expr <~ guard(EOL) | yetExpr)) ^^ { ExprBlock(_) } |
      next ~> figureStr ^^ { Figure(_) }
    ) <~ dedent
  }.named("lang.Block")

  // step blocks
  lazy val stepBlock: Parser[StepBlock] =
    indent ~> (rep1(subStep) ^^ { StepBlock(_) }) <~ dedent

  // user-defined directives
  lazy val directive: Parser[Directive] =
    lazy val name = "[-a-zA-Z0-9]+".r
    ("[" ~> name <~ "=\"") ~ rep1sep(name, ",") <~ "\"]" ^^ {
      case x ~ vs => Directive(x, vs)
    }

  // sub-steps
  lazy val subStepPrefix: Parser[Option[Directive]] =
    next ~ "1." ~> opt(directive) <~ upper
  lazy val subStep: Parser[SubStep] =
    subStepPrefix ~ (step <~ guard(EOL) | yetStep) ^^ {
      case d ~ s => SubStep(d, s)
    }

  // figure string
  lazy val figureStr: P[List[String]] = "<figure>\n".r ~> repsep(
    ".*".r.filter(_.trim != "</figure>"),
    "\n",
  ) <~ "\n *</figure>".r

  // ---------------------------------------------------------------------------
  // metalanguage steps
  // ---------------------------------------------------------------------------
  given step: PL[Step] = {
    letStep |
    setEvalStateStep |
    setStep |
    setFieldsWithIntrinsicsStep |
    performStep |
    performBlockStep |
    returnToResumedStep |
    returnStep |
    assertStep |
    throwStep |
    appendStep |
    prependStep |
    repeatStep |
    pushCtxtStep |
    noteStep |
    suspendStep |
    ifStep |
    forEachArrayIndexStep |
    forEachStep |
    forEachIntStep |
    forEachParseNodeStep |
    resumeStep |
    blockStep
  }.named("lang.Step")

  // let steps
  lazy val letStep: PL[LetStep] =
    ("let" ~> variable <~ "be") ~ opt(
      "the" ~> langType <~ "that is the value of",
    ) ~ endWithExpr ^^ { case x ~ _ ~ e => LetStep(x, e) }

  // set steps
  lazy val setStep: PL[SetStep] =
    ("set" ~> ref <~ ("as" | "to")) ~ endWithExpr ^^ {
      case r ~ e => SetStep(r, e)
    }

  // set fields with intrinsics
  lazy val setFieldsWithIntrinsicsStep: PL[SetFieldsWithIntrinsicsStep] =
    "set fields of" ~> ref <~ (
      "with the values listed in" ~
      "<emu-xref href=\"#table-well-known-intrinsic-objects\"></emu-xref>." ~
      ".*".r
    ) ^^ { SetFieldsWithIntrinsicsStep(_) }

  // if-then-else steps
  lazy val ifStep: PL[IfStep] = {
    ("if" ~> cond <~ "," ~ opt("then")) ~
    (step | yetStep) ~
    opt(
      opt(subStepPrefix) ~
      ("else" | "otherwise") ~ opt(",") ~>
      (step | yetStep),
    )
  } ^^ { case c ~ t ~ e => IfStep(c, t, e) }

  // return steps
  lazy val returnStep: PL[ReturnStep] =
    "return" ~> end ^^! ReturnStep(None) |
    "return" ~> endWithExpr ^^ { case e => ReturnStep(Some(e)) }

  // assertion steps
  lazy val assertStep: PL[AssertStep] =
    "assert" ~ ":" ~> (
      (upper ~> cond <~ end) |
      ".+\\.".r ^^ { case s => ExpressionCondition(YetExpression(s, None)) }
    ) ^^ { AssertStep(_) }

  // for-each steps
  lazy val forEachStep: PL[ForEachStep] =
    lazy val ascending: Parser[Boolean] =
      opt("in reverse List order,") ^^ { !_.isDefined }
    ("for each" ~ opt("element") ~> opt(langType)) ~ variable ~
    ("of" ~> expr) ~ ("," ~> ascending) ~ (opt("do") ~> step) ^^ {
      case t ~ r ~ e ~ a ~ s =>
        ForEachStep(t, r, e, a, s)
    }

  // for-each steps for integers
  lazy val forEachIntStep: PL[ForEachIntegerStep] =
    lazy val interval: Parser[(Expression, Expression)] =
      ("starting with" ~> expr) ~ ("such that" ~ variable ~> (
        "≤" ^^^ { (x: CalcExpression) => x } |
        "<" ^^^ { BinaryExpression(_, BinaryExpressionOperator.Sub, one) }
      ) ~ calcExpr) ^^ {
        case l ~ (f ~ h) => (l, f(h))
      } | "such that" ~> calcExpr ~ ("≤" ~ variable ~ "≤" ~> calcExpr) ^^ {
        case l ~ h => (l, h)
      }
    lazy val ascending: Parser[Boolean] = opt(
      ", in" ~> ("ascending" ^^^ true | "descending" ^^^ false) <~ "order,",
    ) ^^ { _.getOrElse(true) }
    ("for each" ~ "(non-negative )?integer".r ~> variable) ~
    interval ~ ascending ~ (opt("do") ~> step) ^^ {
      case x ~ (low, high) ~ asc ~ body =>
        ForEachIntegerStep(x, low, high, asc, body)
    }

  // for-each steps for array index property
  lazy val forEachArrayIndexStep: PL[ForEachArrayIndexStep] =
    lazy val ascending: Parser[Boolean] =
      opt("in descending numeric index order,") ^^ { !_.isDefined }
    ("for each own property key" ~> variable) ~
    ("of" ~> variable <~ "that is an array index,") ~
    ("whose numeric value is greater than or equal to" ~> expr <~ ",") ~
    ascending ~ (opt("do") ~> step) ^^ {
      case k ~ x ~ s ~ a ~ b => ForEachArrayIndexStep(k, x, s, a, b)
    }

  // for-each steps for parse node
  lazy val forEachParseNodeStep: PL[ForEachParseNodeStep] =
    ("for each child node" ~> variable) ~
    ("of" ~> expr <~ ",") ~ ("do" ~> step) ^^ {
      case x ~ e ~ body => ForEachParseNodeStep(x, e, body)
    }

  // throw steps
  lazy val throwStep: PL[ThrowStep] =
    "throw" ~> errObjLiteral <~ end ^^ { ThrowStep(_) }

  // perform steps
  lazy val performStep: PL[PerformStep] =
    opt("perform" | "call") ~> (invokeExpr | returnIfAbruptExpr) <~ end ^^ {
      PerformStep(_)
    }

  // perform block steps
  lazy val performBlockStep: PL[PerformBlockStep] =
    "perform the following substeps" ~
    "in an implementation-defined order" ~
    ".*".r ~> stepBlock ^^ { PerformBlockStep(_) }

  // append steps
  // NOTE: ("append" ~> expr) ~ ("to" ~> ref) <~ end
  lazy val appendStep: PL[AppendStep] =
    ("append" | "add") ~> expr ~ ((("to" ~ opt("the end of")) |
    "as" ~ ("an" | "the last") ~ "element of" ~ opt("the list")) ~> ref) <~ end
    ^^ { case e ~ r => AppendStep(e, r) }

  // prepend steps
  lazy val prependStep: PL[PrependStep] =
    "prepend" ~> expr ~ ("to" ~> ref) <~ end
    ^^ { case e ~ r => PrependStep(e, r) }

  // repeat steps
  lazy val repeatStep: PL[RepeatStep] =
    ("repeat" ~ ",") ~> opt(("until" | "while") ~> cond <~ ",") ~ step ^^ {
      case c ~ s => RepeatStep(c, s)
    }

  // push context steps
  lazy val pushCtxtStep: PL[PushCtxtStep] =
    "push" ~> ref <~ (
      "onto the execution context stack;" ~ ref ~
      "is now the running execution context" ~ end
    ) ^^ { case r => PushCtxtStep(r) }

  // note steps
  lazy val noteStep: PL[NoteStep] =
    note ^^ { str => NoteStep(str) }

  // suspend steps
  lazy val suspendStep: PL[SuspendStep] =
    lazy val remove: Parser[Boolean] =
      opt("and remove it from the execution context stack") ^^ { _.isDefined }
    "suspend" ~> baseRef ~ (remove <~ end) ^^ {
      case base ~ r => SuspendStep(base, r)
    }

  // set the code evaluation state steps
  lazy val setEvalStateStep: PL[SetEvaluationStateStep] =
    lazy val param: P[Option[Variable]] =
      "for that execution context" ^^^ None |
      "with a" ~ opt(langType) ~> variable ^^ { Some(_) } // TODO handle type
    lazy val body: P[Step] =
      ("the following steps will be performed:" ~> step) |
      // Await
      (", the following steps of the algorithm" ~
      "that invoked Await will be performed," ~
      "with" ~> variable <~ "available" ~ end) ^^ {
        case x => ReturnStep(Some(ReferenceExpression(x)))
      }

    ("set the code evaluation state of" ~> variable) ~
    ("such that when evaluation is resumed" ~> param) ~ body ^^ {
      case c ~ p ~ b => SetEvaluationStateStep(c, p, b)
    }

  // resume the suspended evaluation steps
  lazy val resumeStep: PL[ResumeEvaluationStep] =
    lazy val context: P[Variable] =
      tagged("Resume the suspended evaluation of" ~> variable)
    lazy val arg: P[Option[Expression]] =
      "using" ~> expr <~ "as the result of" ~
      "the operation that suspended it." ^^ { Some(_) } |
      "." ^^^ None
    lazy val param: P[Option[Variable]] =
      "Let" ~> variable <~ (
        "be the" ~
        ("value" | "Completion Record") ~
        "returned by the resumed computation." ~
        guard("\n")
      ) ^^ { Some(_) } |
      guard("\n") ^^^ None
    context ~ arg ~ param ~ rep1(subStep) ^^ {
      case c ~ a ~ p ~ subs => ResumeEvaluationStep(c, a, p, subs)
    }

  // return to resumed steps
  lazy val returnToResumedStep: PL[ReturnToResumeStep] =
    val context: P[Variable] = {
      next ~ "1. NOTE: This returns to the evaluation of the operation" ~
      "that had most previously resumed evaluation of"
    } ~> variable <~ "."
    returnStep ~ context ^^ { case a ~ c => ReturnToResumeStep(c, a) }

  // block steps
  lazy val blockStep: PL[BlockStep] = stepBlock ^^ { BlockStep(_) }

  // not yet supported steps
  lazy val yetStep: PL[YetStep] = yetExpr ^^ { YetStep(_) }

  // end of step
  lazy val note = "NOTE:" ~> ".*".r
  lazy val ignore =
    "(" ~ "see.*\\)".r |
    "as defined in" ~ tagged("") |
    "; that is[^.]*".r
  lazy val end: Parser[String] =
    opt(ignore) ~> "." <~ opt(
      note | ("(" ~ ".*\\)".r) | "This may be.*".r,
    ) ~ upper | ";"

  // end with expression
  lazy val endWithExpr: PL[Expression] = expr <~ end | multilineExpr

  // ---------------------------------------------------------------------------
  // metalanguage expressions
  // ---------------------------------------------------------------------------
  given expr: PL[Expression] = {
    stringConcatExpr |
    listConcatExpr |
    recordExpr |
    lengthExpr |
    substrExpr |
    numberOfExpr |
    sourceTextExpr |
    coveredByExpr |
    getItemsExpr |
    intrExpr |
    clampExpr |
    mathOpExpr |
    bitwiseExpr |
    listExpr |
    xrefExpr |
    soleExpr |
    codeUnitAtExpr |
    invokeExpr |
    calcExpr |
    specialExpr
  }.named("lang.Expression")

  // multilineExpr
  lazy val multilineExpr: PL[MultilineExpression] = closureExpr

  // string concatenation expressions
  lazy val stringConcatExpr: PL[StringConcatExpression] =
    "the string-concatenation of" ~> repsep(expr, sep("and")) ^^ {
      StringConcatExpression(_)
    }

  // list concatenation expressions
  lazy val listConcatExpr: PL[ListConcatExpression] =
    "the list-concatenation of" ~> repsep(expr, sep("and")) ^^ {
      ListConcatExpression(_)
    }

  // record expressions
  lazy val recordExpr: PL[RecordExpression] = {
    opt("the") ~> tname ~
    ("{" ~> repsep((fieldLiteral <~ ":") ~ expr, ",") <~ "}")
  } ^^ {
    case t ~ fs => RecordExpression(t, fs.map { case f ~ e => f -> e })
  } | {
    opt("an " | "a ") ~ ("newly created" | "new") ~
    guard(not("Realm")) ~> tname <~ opt(
      "containing no bindings" |
      "with no fields" |
      "that initially has no fields",
    )
  } ^^ {
    case t => RecordExpression(t, List())
  }

  // `length of` expressions
  lazy val lengthExpr: PL[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) } |
    "the number of code" ~
    ("units" | "unit elements") ~ "in" ~> expr ^^ {
      LengthExpression(_)
    }

  // `substring of` expressions
  lazy val substrExpr: PL[SubstringExpression] =
    ("the substring of" ~> expr) ~
    ("from" ~> expr) ~
    opt("to" ~> expr) ^^ { case e ~ f ~ t => SubstringExpression(e, f, t) }

  // `the number of elements in` expressions
  lazy val numberOfExpr: PL[NumberOfExpression] =
    ("the number of elements" ~ ("in" | "of") ~ opt("the List") ~> expr) ^^ {
      NumberOfExpression(_)
    }

  // `source text` expressions
  lazy val sourceTextExpr: PL[SourceTextExpression] =
    ("the source text matched by" ~> expr) ^^ {
      case e =>
        SourceTextExpression(e)
    }

  // `covered by` expressions
  lazy val coveredByExpr: PL[CoveredByExpression] =
    "the" ~> expr ~ ("that is covered by" ~> expr) ^^ {
      case r ~ c => CoveredByExpression(c, r)
    }

  // get items ast expressions
  lazy val getItemsExpr: PL[GetItemsExpression] =
    ("the List of" ~> expr <~ "items") ~
    ("in" ~> expr <~ "," ~ "in source text order") ^^ {
      case t ~ e => GetItemsExpression(t, e)
    }

  // abstract closure expressions
  lazy val closureExpr: PL[AbstractClosureExpression] =
    lazy val params: P[List[Variable]] =
      "no parameters" ^^^ Nil |
      "parameters" ~> ("(" ~> repsep(variable, ",") <~ ")")
    lazy val captured: P[List[Variable]] =
      "captures nothing" ^^^ Nil |
      "captures" ~> repsep(variable, sep("and"))

    "a new" ~ opt(
      "Job",
    ) ~ "Abstract Closure with" ~> params ~ ("that" ~> captured) ~
    ("and performs the following steps when called:" ~> blockStep) ^^ {
      case ps ~ cs ~ body => AbstractClosureExpression(ps, cs, body)
    }

  // intrinsic expressions
  lazy val intrExpr: PL[IntrinsicExpression] =
    intr ^^ { IntrinsicExpression(_) }

  // base calculation expressione
  lazy val baseCalcExpr: PL[CalcExpression] =
    (baseCalcExpr ~ ("<sup>" ~> calcExpr <~ "</sup>")) ^^ {
      case b ~ e => ExponentiationExpression(b, e)
    } |
    returnIfAbruptExpr |
    convExpr |
    mathFuncExpr |
    "(" ~> calcExpr <~ ")" |
    refExpr |
    literal

  // calculation expressions
  lazy val calcExpr: PL[CalcExpression] = {
    import BinaryExpressionOperator.*
    import UnaryExpressionOperator.*

    lazy val unary: PL[CalcExpression] =
      (("-" | "the result of negating") ^^^ Neg) ~
      baseCalcExpr ^^ { case o ~ e => UnaryExpression(o, e) } |
      baseCalcExpr

    lazy val term: PL[CalcExpression] = unary ~ rep(
      ("×" ^^^ Mul | "/" ^^^ Div | "modulo" ^^^ Mod) ~ unary,
    ) ^^ {
      case l ~ rs =>
        rs.foldLeft(l) { case (l, op ~ r) => BinaryExpression(l, op, r) }
    }

    lazy val calc: PL[CalcExpression] = term ~ rep(
      ("+" ^^^ Add | "-" ^^^ Sub) ~ term,
    ) ^^ {
      case l ~ rs =>
        rs.foldLeft(l) { case (l, op ~ r) => BinaryExpression(l, op, r) }
    }

    calc
  }

  // conversion expressions
  lazy val convExpr: PL[ConversionExpression] =
    import ConversionExpressionOperator.*
    lazy val opFormat = (
      "𝔽" ^^^ ToNumber | "ℤ" ^^^ ToBigInt | "ℝ" ^^^ ToMath
    ) ~ ("(" ~> expr <~ ")")
    lazy val textFormat =
      ("the " | "an " | "a ") ~> (
        "implementation-approximated Number" ^^^ ToApproxNumber |
        "Number" ^^^ ToNumber |
        "BigInt" ^^^ ToBigInt |
        opt("integer that is the") ~ "numeric" ^^^ ToMath
      ) ~
      ("value" ~ ("of" | "for" | "representing" | "that represents") ~> expr) <~
      opt(textFormatPostfix)
    lazy val textFormatPostfix = opt(",") ~ ("rounded" | "rounding") ~ "[^.]+".r
    (opFormat | textFormat) ^^ { case op ~ e => ConversionExpression(op, e) }

  // emu-xref expressions
  // TODO cleanup spec.html
  lazy val xrefExpr: PL[XRefExpression] =
    import XRefExpressionOperator.*
    lazy val xrefOp: P[XRefExpressionOperator] = {
      "specified in" |
      "described in" |
      "the definition specified in" |
      "the algorithm steps defined in" |
      "the ordinary object internal method defined in"
    } ^^^ Algo | {
      "the internal slots listed in"
    } ^^^ InternalSlots | {
      "the number of non-optional parameters of" ~
      "the function definition in"
    } ^^^ ParamLength
    xrefOp ~ (
      "<emu-xref href=\"#" ~> "[a-z-.]+".r <~ "\"[a-z ]*>".r ~ tagEnd
    ) ^^ { case op ~ id => XRefExpression(op, id) }

  // the sole element expressions
  lazy val soleExpr: PL[SoleElementExpression] =
    ("the sole element of" | "the string that is the only element of")
    ~> expr ^^ { SoleElementExpression(_) }

  // reference expressions
  lazy val refExpr: PL[ReferenceExpression] = ref ^^ { ReferenceExpression(_) }

  // mathematical operation expressions
  lazy val mathFuncExpr: PL[MathFuncExpression] =
    import MathFuncExpressionOperator.*
    (
      "max" ^^^ Max | "min" ^^^ Min |
      "abs" ^^^ Abs | "floor" ^^^ Floor
    ) ~ ("(" ~> repsep(calcExpr, ",") <~ ")") ^^ {
      case o ~ as =>
        MathFuncExpression(o, as)
    }

  // literals
  // GetIdentifierReference uses 'the value'
  lazy val literal: PL[Literal] = opt("the" ~ opt(langType) ~ "value") ~> (
    opt("the") ~> "*this* value" ^^! ThisLiteral() |
    "this" ~ ("Parse Node" | ntLiteral) ^^! ThisLiteral() |
    "NewTarget" ^^! NewTargetLiteral() |
    hexLiteral |
    "`[^`]+`".r ^^ { case s => CodeLiteral(s.substring(1, s.length - 1)) } |
    ntLiteral |
    "~" ~> "[-+a-zA-Z0-9]+".r <~ "~" ^^ { ConstLiteral(_) } |
    "the empty String" ^^! StringLiteral("") |
    strLiteral <~ opt("\\([^)]*\\)".r) |
    fieldLiteral |
    errObjLiteral |
    "@@" ~> word ^^ { SymbolLiteral(_) } |
    "+∞" ^^! PositiveInfinityMathValueLiteral() |
    "-∞" ^^! NegativeInfinityMathValueLiteral() |
    opt(int) ~ "π" ^^ {
      case p ~ n => MathConstantLiteral(p.getOrElse(1), n)
    } |
    decimal ^^ { DecimalMathValueLiteral(_) } |
    "*+∞*<sub>𝔽</sub>" ^^! NumberLiteral(Double.PositiveInfinity) |
    "*-∞*<sub>𝔽</sub>" ^^! NumberLiteral(Double.NegativeInfinity) |
    "*NaN*" ^^! NumberLiteral(Double.NaN) |
    "*" ~> double <~ "*<sub>𝔽</sub>" ^^ { NumberLiteral(_) } |
    "*" ~> bigInt <~ "*<sub>ℤ</sub>" ^^ { BigIntLiteral(_) } |
    "*true*" ^^! TrueLiteral() |
    "*false*" ^^! FalseLiteral() |
    "*undefined*" ^^! UndefinedLiteral() |
    "*null*" ^^! NullLiteral() |
    "absent" ^^! AbsentLiteral() |
    "Undefined" ^^! UndefinedTypeLiteral() |
    "Null" ^^! NullTypeLiteral() |
    "Boolean" ^^! BooleanTypeLiteral() |
    "String" ^^! StringTypeLiteral() |
    "Symbol" ^^! SymbolTypeLiteral() |
    "Number" ^^! NumberTypeLiteral() |
    "BigInt" ^^! BigIntTypeLiteral() |
    "Object" ^^! ObjectTypeLiteral()
  )

  // field literal
  lazy val fieldLiteral: PL[FieldLiteral] =
    "[[" ~> word <~ "]]" ^^ { FieldLiteral(_) }

  // code unit literals with hexadecimal numbers
  lazy val hexLiteral: PL[HexLiteral] =
    (opt("the code unit") ~ "0x" ~> "[0-9A-F]+".r) ~
    opt("(" ~> "[ A-Z]+".r <~ ")") ^^ {
      case n ~ x =>
        HexLiteral(Integer.parseInt(n, 16), x)
    }

  // nonterminal literals
  lazy val ntLiteral: PL[NonterminalLiteral] =
    lazy val flags: P[List[String]] =
      "[" ~> repsep("^[~+][A-Z][a-z]+".r, ",") <~ "]" | "" ^^^ Nil
    opt("the grammar symbol" | "the") ~> opt(ordinal) ~
    ("|" ~> word <~ opt("?")) ~ flags <~ "|" ^^ {
      case ord ~ x ~ fs => NonterminalLiteral(ord, x, fs)
    }

  // string literals
  lazy val strLiteral: PL[StringLiteral] =
    opt("the String") ~> """\*"[^"]*"\*""".r ^^ {
      case s =>
        val str = s
          .substring(2, s.length - 2)
          .replace("\\*", "*")
          .replace("\\\\", "\\")
        StringLiteral(str)
    }

  // production literals
  // XXX need to be generalized?
  private lazy val prodLiteral: PL[ProductionLiteral] =
    tagged((word <~ ":") ~ ("[\\[\\]A-Za-z]+".r)) ^^ {
      case l ~ r => ProductionLiteral(l, r)
    }

  // error object literals
  lazy val errObjLiteral: PL[ErrorObjectLiteral] =
    lazy val errorName = "*" ~> word.filter(_.endsWith("Error")) <~ "*" ^^ {
      ErrorObjectLiteral(_)
    }
    "a newly created" ~> errorName <~ "object" | "a" ~> errorName <~ "exception"

  // clamping expression
  lazy val clampExpr: PL[ClampExpression] =
    "the result of clamping" ~> expr ~
    ("between" ~> expr) ~ ("and" ~> expr) ^^ {
      case t ~ l ~ u => ClampExpression(t, l, u)
    }

// mathematical operation expressions
  lazy val mathOpExpr: PL[MathOpExpression] =
    opt("the result of") ~ opt("the") ~> {
      import MathOpExpressionOperator.*
      "negation of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Neg, List(e))
      } | ("sum of" ~> baseCalcExpr) ~ ("and" ~> baseCalcExpr) ^^ {
        case l ~ r => MathOpExpression(Add, List(l, r))
      } | ("product of" ~> baseCalcExpr) ~ ("and" ~> baseCalcExpr) ^^ {
        case l ~ r => MathOpExpression(Mul, List(l, r))
      } | ("difference" ~> baseCalcExpr) ~ ("minus" ~> baseCalcExpr) ^^ {
        case l ~ r => MathOpExpression(Sub, List(l, r))
      } | (baseCalcExpr) ~ ("raised to the power" ~> baseCalcExpr) ^^ {
        case l ~ r => MathOpExpression(Pow, List(l, r))
      } | ("raising" ~> baseCalcExpr) ~ ("to the" ~> baseCalcExpr <~ "power") ^^ {
        case l ~ r => MathOpExpression(Pow, List(l, r))
      } | "subtracting 1 from the exponential function of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Expm1, List(e))
      } | "base 10 logarithm of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Log10, List(e))
      } | "base 2 logarithm of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Log2, List(e))
      } | "cosine of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Cos, List(e))
      } | "cube root of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Cbrt, List(e))
      } | "exponential function of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Exp, List(e))
      } | "hyperbolic cosine of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Cosh, List(e))
      } | "hyperbolic sine of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Sinh, List(e))
      } | "hyperbolic tangent of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Tanh, List(e))
      } | "inverse cosine of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Acos, List(e))
      } | "inverse hyperbolic cosine of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Acosh, List(e))
      } | "inverse hyperbolic sine of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Asinh, List(e))
      } | "inverse hyperbolic tangent of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Atanh, List(e))
      } | "inverse sine of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Asin, List(e))
      } | ("inverse tangent of the quotient" ~> baseCalcExpr) ~ ("/" ~> baseCalcExpr) ^^ {
        case x ~ y => MathOpExpression(Atan2, List(x, y))
      } | "inverse tangent of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Atan, List(e))
      } | "natural logarithm of 1 +" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Log1p, List(e))
      } | "natural logarithm of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Log, List(e))
      } | "sine of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Sin, List(e))
      } | "square root of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Sqrt, List(e))
      } | "tangent of" ~> baseCalcExpr ^^ {
        case e => MathOpExpression(Tan, List(e))
      }
    }

  // bitwise expressions
  lazy val bitwiseExpr: PL[BitwiseExpression] =
    import BitwiseExpressionOperator.*
    val op: Parser[BitwiseExpressionOperator] =
      "bitwise AND" ^^^ BAnd |
      "bitwise inclusive OR" ^^^ BOr |
      "bitwise exclusive OR (XOR)" ^^^ BXOr
    ("the result of applying the" ~> op) ~
    ("operation to" ~> expr) ~
    ("and" ~> expr) ^^ { case op ~ l ~ r => BitwiseExpression(l, op, r) }

  // metalanguage invocation expressions
  lazy val invokeExpr: PL[InvokeExpression] = tagged {
    invokeAOExpr |
    invokeNumericExpr |
    invokeClosureExpr |
    invokeAMExpr |
    invokeSDOExpr
  }

  // arguments for invocation epxressions
  lazy val invokeArgs: P[List[Expression]] = ("(" ~> repsep(expr, ",") <~ ")")

  // abstract operation (AO) invocation expressions
  lazy val invokeAOExpr: PL[InvokeAbstractOperationExpression] =
    "(this)?[A-Z][a-zA-Z0-9/]*".r ~ invokeArgs ^^ {
      case x ~ as => InvokeAbstractOperationExpression(x, as)
    }

  // numeric method invocation expression
  lazy val invokeNumericExpr: PL[InvokeNumericMethodExpression] =
    numericName ~ ("::" ~> "[A-Za-z]+".r) ~ invokeArgs ^^ {
      case t ~ op ~ as => InvokeNumericMethodExpression(t, op, as)
    }
  lazy val numericName: Parser[String] = "Number" | "BigInt"

  // abstract closure invocation expression
  lazy val invokeClosureExpr: PL[InvokeAbstractClosureExpression] =
    variable ~ invokeArgs ^^ {
      case v ~ as =>
        InvokeAbstractClosureExpression(v, as)
    }

  // method invocation expressions
  lazy val invokeAMExpr: PL[InvokeMethodExpression] =
    // handle emu-meta tag
    tagged(propRef) ~ invokeArgs ^^ {
      case p ~ as => InvokeMethodExpression(p, as)
    }

  // syntax-directed operation (SDO) invocation expressions
  lazy val invokeSDOExpr: PL[InvokeSyntaxDirectedOperationExpression] =
    lazy val name =
      (opt("the result of performing" | "the result of" | "the") ~ guard(
        not(component),
      ) ~> camel)
    lazy val base = ("of" ~> expr)
    lazy val argsPart =
      "with" ~ ("arguments" | "argument") ~> repsep(expr, sep("and"))

    // normal SDO
    lazy val normalSDOExpr =
      name ~ base ~ opt(argsPart) ^^ {
        case x ~ b ~ as =>
          InvokeSyntaxDirectedOperationExpression(b, x, as.getOrElse(Nil))
      }

    // Evaluation SDO
    lazy val evalSDOExpr =
      "the result of evaluating" ~ opt(langType <~ guard(expr)) ~> expr ^^ {
        case b =>
          InvokeSyntaxDirectedOperationExpression(b, "Evaluation", Nil)
      }

    // Contains SDO
    lazy val containsSDOExpr =
      expr ~ ("Contains" ~> expr) ^^ {
        case b ~ arg =>
          InvokeSyntaxDirectedOperationExpression(b, "Contains", List(arg))
      }

    normalSDOExpr | evalSDOExpr | containsSDOExpr

  // return-if-abrupt expressions
  lazy val returnIfAbruptExpr: PL[ReturnIfAbruptExpression] =
    ("?" ^^^ true | "!" ^^^ false) ~ expr ^^ {
      case c ~ e => ReturnIfAbruptExpression(e, c)
    }

  // list expressions
  lazy val listExpr: PL[ListExpression] =
    "a new empty List" ^^! ListExpression(Nil) |
    "«" ~> repsep(expr, ",") <~ "»" ^^ { ListExpression(_) } |
    "a List whose sole element is" ~> expr ^^ { e => ListExpression(List(e)) }

  // the code unit expression at specific index of a string
  lazy val codeUnitAtExpr: PL[CodeUnitAtExpression] =
    ("the code unit at index" ~> expr) ~
    ("within" ~ opt("the String") ~> expr) ^^ {
      case i ~ b => CodeUnitAtExpression(b, i)
    }

  // rarely used expressions
  lazy val specialExpr: PL[Expression] =
    import ConversionExpressionOperator.*
    // ClassStaticBlockDefinitionEvaluation
    "the empty sequence of Unicode code points" ^^! StringLiteral("") |
    // Array.prototype.join
    "the single-element String" ~> strLiteral |
    // MethodDefinitionEvaluation, ClassFieldDefinitionEvaluation
    "an instance of the production" ~> prodLiteral |
    // NumberBitwiseOp
    "the 32-bit two's complement bit string representing" ~> expr

  // not yet supported expressions
  lazy val yetExpr: PL[YetExpression] =
    opt("[YET]") ~> ".+".r ~ opt(block) ^^ { case s ~ b => YetExpression(s, b) }

  // ---------------------------------------------------------------------------
  // metalanguage conditions
  // ---------------------------------------------------------------------------
  given cond: PL[Condition] = {
    import CompoundConditionOperator.*

    // get compound condition from base and operation
    def compound(
      base: P[Condition],
      op: Parser[CompoundConditionOperator],
    ): Parser[Condition] =
      rep(base <~ opt(",")) ~ op ~ (opt("if") ~> base) ^^ {
        case ls ~ op ~ r =>
          ls.foldRight(r) {
            case (l, r) => CompoundCondition(l, op, r)
          }
      }

    lazy val simpleAnd: P[Condition] = compound(baseCond, "and" ^^^ And)
    lazy val simpleOr: P[Condition] = compound(baseCond, "or" ^^^ Or)
    lazy val simpleImply: P[Condition] =
      "If" ~> compound(baseCond, "then" ^^^ Imply)
    lazy val compOr: P[Condition] = compound(simpleAnd, "or" ^^^ Or)

    baseCond ||| simpleAnd ||| simpleOr ||| simpleImply ||| compOr
  }.named("lang.Condition")

  // base conditions
  lazy val baseCond: PL[Condition] =
    exprCond |||
    instanceOfCond |||
    hasFieldCond |||
    hasBindingCond |||
    productionCond |||
    predCond |||
    isAreCond |||
    binCond |||
    inclusiveIntervalCond |||
    containsWhoseCond |||
    specialCond

  // expression conditions
  lazy val exprCond: PL[ExpressionCondition] = expr ^^ {
    ExpressionCondition(_)
  }

  // instance check conditions
  lazy val instanceOfCond: PL[InstanceOfCondition] =
    expr ~ isEither(singleLangType) ^^ {
      case e ~ (n ~ t) => InstanceOfCondition(e, n, t)
    }

  // field includsion conditions
  lazy val hasFieldCond: PL[HasFieldCondition] =
    lazy val fieldStr = "field" | ("internal" ~ ("method" | "slot"))
    // GeneratorValidate
    (ref <~ opt("also")) ~
    ("has" ^^^ false | "does not have" ^^^ true) ~
    (("an " | "a ") ~> expr <~ fieldStr) ^^ {
      case r ~ n ~ f => HasFieldCondition(r, n, f)
    }

  // binding includsion conditions
  lazy val hasBindingCond: PL[HasBindingCondition] =
    // GeneratorValidate
    (ref <~ opt("also")) ~
    ("has" ^^^ false ||| "does not have" ^^^ true) ~
    ("a binding for" ~> expr) ^^ {
      case r ~ n ~ f => HasBindingCondition(r, n, f)
    }

  // production conditions
  // Ex: If _x_ is <emu-grammar>Statement : LabelledStatement</emu-grammar>, ...
  lazy val productionCond: PL[ProductionCondition] =
    (expr <~ "is") ~ prodLiteral ^^ {
      case nt ~ prod => ProductionCondition(nt, prod.lhs, prod.rhs) // TODO
    }

  // predicate conditions
  lazy val predCond: PL[PredicateCondition] =
    import PredicateConditionOperator.*
    lazy val op: Parser[PredicateConditionOperator] =
      "finite" ^^^ Finite |
      "an abrupt completion" ^^^ Abrupt |
      "a normal completion" ^^^ Normal |
      "never an abrupt completion" ^^^ NeverAbrupt |
      "duplicate entries" ^^^ Duplicated |
      "present" ^^^ Present |
      ("empty" | "an empty List") ^^^ Empty |
      "strict mode code" ^^^ StrictMode |
      "an array index" ^^^ ArrayIndex |
      "a non-negative integral Number" ^^^ NonNegative |
      "the token `false`" ^^^ FalseToken |
      "the token `true`" ^^^ TrueToken |
      "a data property" ^^^ DataProperty |
      "an accessor property" ^^^ AccessorProperty |
      "a fully populated Property Descriptor" ^^^ FullyPopulated |
      "an instance of a nonterminal" ^^^ Nonterminal |
      "an integral Number" ^^^ IntegralNumber

    lazy val neg: Parser[Boolean] =
      isNeg | ("contains" | "has") ~> ("any" ^^^ false | "no" ^^^ true)

    expr ~ neg ~ op ^^ {
      case r ~ n ~ o => PredicateCondition(r, n, o)
    }

  // `A is/are B` condition
  lazy val isAreCond: PL[IsAreCondition] =
    lazy val left: P[List[Expression]] =
      (opt("both") ~> expr) ~ ("and" ~> expr) <~ guard("are") ^^ {
        case e0 ~ e1 => List(e0, e1)
      } |
      expr <~ guard("is") ^^ { List(_) }

    lazy val neg: P[Boolean] = isNeg | areNeg
    lazy val right: P[Boolean ~ List[Expression]] = either(neg, expr)

    left ~ right ^^ { case l ~ (n ~ r) => IsAreCondition(l, n, r) }

  // binary conditions
  lazy val binCond: PL[BinaryCondition] =
    import BinaryConditionOperator.*
    lazy val op: Parser[BinaryConditionOperator] =
      "≠" ^^^ NEq |
      "=" ^^^ Eq |
      "≤" ^^^ LessThanEqual |
      "<" ^^^ LessThan |
      "≥" ^^^ GreaterThanEqual |
      ">" ^^^ GreaterThan |
      "is the same sequence of code units as" ^^^ SameCodeUnits |
      "contains" ^^^ Contains |
      "does not contain" ^^^ NContains
    expr ~ op ~ expr ^^ { case l ~ o ~ r => BinaryCondition(l, o, r) } |||
    expr ~ (isNeg <~ (opt("currently") ~> "an element of")) ~ expr ^^ {
      case l ~ n ~ r =>
        BinaryCondition(r, if (n) NContains else Contains, l)
    }

  // binary conditions
  lazy val inclusiveIntervalCond: PL[InclusiveIntervalCondition] = {
    (expr <~ "is") ~
    opt("not") ~
    ("in the inclusive interval from" ~> expr) ~
    ("to" ~> expr)
  } ^^ {
    case l ~ n ~ f ~ t => InclusiveIntervalCondition(l, n.isDefined, f, t)
  }

  // contains-whose conditions
  lazy val containsWhoseCond: PL[ContainsWhoseCondition] =
    expr ~
    ("contains" ~> langType) ~
    ("whose" ~ "[[" ~> word <~ "]]") ~
    ("is" ~> expr) ^^ {
      case l ~ t ~ f ~ e => ContainsWhoseCondition(l, t, f, e)
    }

  // rarely used conditions
  // TODO clean-up
  lazy val specialCond: PL[Condition] = {
    // ResolveBinding
    "the source text matched by the syntactic production" ~
    "that is being evaluated is contained in strict mode code"
  } ^^! getExprCond(TrueLiteral()) | {
    // Script.IsStrict (assume strict)
    "the Directive Prologue of |ScriptBody| contains a Use Strict Directive"
  } ^^! getExprCond(TrueLiteral()) | {
    // PropertyDefinition[2,0].PropertyDefinitionEvaluation
    "this |PropertyDefinition| is contained within" ~
    "a |Script| that is being evaluated for JSON.parse" ~
    ignore ~ guard(",")
  } ^^! getExprCond(FalseLiteral()) | {
    // CreatePerIterationEnvironment
    expr <~ "has any elements"
  } ^^ { PredicateCondition(_, true, PredicateConditionOperator.Empty) } | {
    // ForBodyEvaluation
    expr ~ isNeg <~ "~[empty]~"
  } ^^ {
    case e ~ n => PredicateCondition(e, !n, PredicateConditionOperator.Present)
  } | {
    // %ForInIteratorPrototype%.next
    ("there does not exist an element" ~ variable ~ "of" ~> variable) ~
    ("such that SameValue(" ~> variable <~ "," ~ variable ~ ") is *true*")
  } ^^ {
    case list ~ elem =>
      BinaryCondition(
        getRefExpr(list),
        BinaryConditionOperator.NContains,
        getRefExpr(elem),
      )
  } | {
    // CallExpression[0,0].Evaluation
    expr <~ "has no elements"
  } ^^ { PredicateCondition(_, false, PredicateConditionOperator.Empty) } | {
    // ArraySpeciesCreate, SameValueNonNumeric
    expr ~ ("and" ~> expr) ~ areNeg <~ "the same" ~ opt(langType) ~ opt("value")
  } ^^ { case l ~ r ~ n => IsAreCondition(List(l), n, List(r)) } | {
    // SameValueNonNumeric, GeneratorValidate
    expr ~ (isNeg <~ ("the same" ~ opt(langType) ~ opt("value") ~ "as")) ~ expr
  } ^^ { case l ~ n ~ r => IsAreCondition(List(l), n, List(r)) } | {
    // SameValue
    expr ~ (isNeg <~ "different from" ^^ { !_ }) ~ expr
  } ^^ { case l ~ n ~ r => IsAreCondition(List(l), n, List(r)) } | {
    // IsLessThan
    (variable <~ "or") ~ variable ~ isNeg ~ literal
  } ^^ {
    case v0 ~ v1 ~ n ~ e =>
      CompoundCondition(
        IsAreCondition(List(getRefExpr(v0)), n, List(e)),
        CompoundConditionOperator.Or,
        IsAreCondition(List(getRefExpr(v1)), n, List(e)),
      )
  } | {
    ref ~ ("is" ^^^ false | "is not" ^^^ true) <~ "a strict binding"
  } ^^ {
    case r ~ n =>
      val ref = PropertyReference(r, FieldProperty("strict"))
      IsAreCondition(List(ReferenceExpression(ref)), n, List(TrueLiteral()))
  } | {
    ref ~ ("has been" ^^^ false | "has not" ~ opt("yet") ~ "been" ^^^ true) <~
    "initialized"
  } ^^ {
    case r ~ n =>
      val ref = PropertyReference(r, FieldProperty("initialized"))
      IsAreCondition(List(ReferenceExpression(ref)), n, List(TrueLiteral()))
  }

  // ---------------------------------------------------------------------------
  // metalanguage references
  // ---------------------------------------------------------------------------
  given ref: PL[Reference] = {
    baseRef ||| propRef ||| specialRef
  }.named("lang.Reference")

  // property references
  lazy val propRef: PL[PropertyReference] = opt("the value of") ~> {
    baseRef ~ rep1(prop) ^^ {
      case base ~ ps =>
        val (p :: rest) = ps
        rest.foldLeft[PropertyReference](PropertyReference(base, p))(
          PropertyReference(_, _),
        )
    } ||| prop ~ baseRef ^^ {
      case p ~ base => PropertyReference(base, p)
    }
  }

  // base references
  lazy val baseRef: PL[Reference] =
    variable |
    "the" ~ opt("currently") ~ "running execution context" ^^! {
      RunningExecutionContext()
    } | "the current Realm Record" ^^! {
      CurrentRealmRecord()
    } | ("the active function object" | "the active function") ^^! {
      ActiveFunctionObject()
    } | "the second to top element of the execution context stack" ^^! {
      SecondExecutionContext()
    }

  // variables
  lazy val variable: PL[Variable] = "_[^_]+_".r ^^ {
    case s => Variable(s.substring(1, s.length - 1))
  }

  // special reference
  lazy val specialRef: P[Reference] = {
    // IsLessThan
    "the" ~> variable <~ "flag"
  } | {
    // GetPrototypeFromConstructor
    (variable <~ "'s intrinsic object named") ~ variable
  } ^^ {
    case realm ~ v =>
      val intrBase = PropertyReference(realm, FieldProperty("Intrinsics"))
      PropertyReference(intrBase, IndexProperty(getRefExpr(v)))
  } | {
    // OrdinaryGetOwnProperty
    ("the value of" ~> variable <~ "'s") ~ ("[[" ~> word <~ "]]" ~ "attribute")
  } ^^ { case v ~ a => PropertyReference(v, FieldProperty(a)) } | {
    // Set.prototype.add
    ("the List that is" ~> propRef)
  } | {
    // AsyncGeneratorCompleteStep
    ("the" ~> ordinal <~ "element") ~ ("of" ~> variable)
  } ^^ {
    case o ~ x =>
      PropertyReference(x, IndexProperty(DecimalMathValueLiteral(o - 1)))
  } | {
    // SetFunctionName, SymbolDescriptiveString
    (variable <~ "'s") ~ ("[[" ~> word <~ "]]") <~ "value"
  } ^^ { case b ~ f => PropertyReference(b, FieldProperty(f)) }

  // ---------------------------------------------------------------------------
  // metalanguage properties
  // ---------------------------------------------------------------------------
  given prop: PL[Property] = preProp | postProp
  lazy val preProp: PL[Property] = {
    "the" ~> component <~ opt("component") ~ "of" ^^ {
      ComponentProperty(_)
    } |||
    "the binding for" ~> expr <~ "in" ^^ { BindingProperty(_) } |||
    "the" ~> nt <~ "of" ^^ { NonterminalProperty(_) }
  }.named("lang.Property")

  lazy val postProp: PL[Property] = {
    "." ~> "[[" ~> word <~ "]]" ^^ { FieldProperty(_) } |||
    "." ~ "[[" ~> intr <~ "]]" ^^ { i => IntrinsicProperty(i) } |||
    ("'s" | ".") ~> camel ^^ { ComponentProperty(_) } |||
    "[" ~> expr <~ "]" ^^ { IndexProperty(_) }
  }.named("lang.Property")

  // TODO extract component name from spec.html
  lazy val component: Parser[String] =
    "LexicalEnvironment" |
    "Function" |
    "Generator" |
    "PrivateEnvironment" |
    "Realm" |
    "ScriptOrModule" |
    "VariableEnvironment" |
    "value"

  // ---------------------------------------------------------------------------
  // metalanguage intrinsics
  // ---------------------------------------------------------------------------
  given intr: PL[Intrinsic] = {
    opt("the intrinsic function") ~ "%" ~> (word ~ rep("." ~> word)) <~ "%" ^^ {
      case b ~ ps => Intrinsic(b, ps)
    }
  }.named("lang.Intrinsic")

  // ---------------------------------------------------------------------------
  // metalanguage types
  // ---------------------------------------------------------------------------
  // metalanguage types with unknown types
  given langTypeWithUnknown: PL[Type] = {
    (
      langTy <~ guard(opt(",") ~ EOL | "[_:]".r) |
      unknownTy
    ) ^^ { Type(_) }
  }.named("lang.Type")

  /** Metalanguage Types
    *
    * Reference: https://github.com/tc39/ecmarkup/blob/main/src/type-parser.ts
    */
  lazy val langType: PL[Type] = {
    langTy ^^ { Type(_) }
  }.named("lang.Type")

  lazy val singleLangType: PL[Type] = {
    singleLangTy ^^ { Type(_) }
  }.named("lang.Type (single)")

  // types
  lazy val langTy: P[Ty] = multi(valueTy, either = false) | specialTy
  lazy val singleLangTy: P[Ty] = singleValueTy | specialTy

  // unknown types
  lazy val unknownTy: P[Ty] = "([^,_]|, )+".r ^^ {
    case "unknown" => UnknownTy(None)
    case s         => UnknownTy(Some(s.trim))
  }

  // value types
  lazy val valueTy: P[ValueTy] = multi(singleValueTy)
  lazy val singleValueTy: P[ValueTy] = singleCompTy | singlePureValueTy

  // completion record types
  lazy val compTy: P[ValueTy] = multi(singleCompTy)
  lazy val singleCompTy: P[ValueTy] =
    "a Completion Record" ^^^ CompT |
    "a normal completion containing" ~> pureValueTy ^^ { NormalT(_) } |
    "a normal completion" ^^^ NormalT |
    "a throw completion" ^^^ AbruptT("throw") |
    "a return completion" ^^^ AbruptT("return") |
    "an abrupt completion" ^^^ AbruptT

  // pure value types
  lazy val pureValueTy: P[ValueTy] = multi(singlePureValueTy)
  lazy val singlePureValueTy: P[ValueTy] = nameTy | recordTy | listTy | simpleTy

  // named record types
  lazy val nameTy: P[ValueTy] =
    opt("an " | "a ") ~> rep1("[-a-zA-Z]+".r.filter(_ != "or")).flatMap {
      case ss =>
        val name = ss.mkString(" ")
        val normalizedName = Type.normalizeName(name)
        if (TyModel.es.infos.contains(normalizedName)) success(NameT(name))
        else failure("unknown type name")
    }

  // record types TODO
  lazy val recordTy: P[ValueTy] = failure("TODO")

  // list types
  lazy val listTy: P[ValueTy] =
    opt("an " | "a ") ~ "List of" ~> pureValueTy ^^ { ListT(_) }

  // simple types
  lazy val simpleTy: P[ValueTy] = opt("an " | "a ") ~> {
    "Number" ^^^ NumberT |
    "BigInt" ^^^ BigIntT |
    "Boolean" ^^^ BoolT |
    "Symbol" ^^^ SymbolT |
    "String" ^^^ StrT |
    "Object" ^^^ ObjectT |
    "*undefined*" ^^^ UndefT |
    "*null*" ^^^ NullT |
    "*true*" ^^^ TrueT |
    "*false*" ^^^ FalseT |
    "ECMAScript language value" ^^^ ESValueT |
    "property key" ^^^ (StrT || SymbolT) |
    "Parse Node" ^^^ AstT |
    nt <~ "Parse Node" ^^ { AstT(_) } |
    "~" ~> "[-+a-zA-Z0-9]+".r <~ "~" ^^ { ConstT(_) }
  } <~ opt("s")

  private def multi(parser: P[ValueTy], either: Boolean = true): P[ValueTy] =
    val multiParser = (if (either) "either" else "") ~> {
      rep1sep(parser, ",") ~ (sep("or") ~> parser)
    } ^^ { case ts ~ t => ts.foldLeft(t)(_ || _) }
    multiParser | parser

  // rarely used expressions
  lazy val specialTy: P[Ty] = opt("an " | "a ") ~> {
    "List of" ~> word ^^ {
      case s => UnknownTy(s"List of $s")
    } | "Record" ~ "{" ~> repsep(fieldLiteral, ",") <~ "}" ^^ {
      case fs => UnknownTy(s"Record { ${fs.mkString(", ")} }")
    } | (nt | tname) ^^ {
      case s => UnknownTy(s)
    }
  }

  // type name
  lazy val tname: P[String] =
    rep1(camel) ^^ {
      case ss => ss.mkString(" ")
    } |||
    (opt("ECMAScript code") ~ "execution context" ^^^ "ExecutionContext" |
    "\\w+ Environment Record".r |
    "[a-zA-Z ]+ object".r)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // html tags
  private lazy val tagStart: Parser[String] = "<[^>]+>".r
  private lazy val tagEnd: Parser[String] = "</[a-z-]+>".r
  private def tagged[T](parser: Parser[T]): Parser[T] =
    opt(tagStart) ~> parser <~ opt(tagEnd)

  // nonterminals
  private lazy val nt: Parser[String] = "|" ~> word <~ "|"

  // ordinal
  private lazy val ordinal: Parser[Int] =
    word.map(_.toIntFromOrdinal).filter(_.isDefined).map(_.get)

  // separators
  private def sep(s: Parser[Any]): Parser[Any] = (
    "," ~ s | s | ","
  )

  // verbs
  private def either[T](
    b: Parser[Boolean],
    p: Parser[T],
  ): Parser[Boolean ~ List[T]] =
    lazy val compoundGuard = guard(not("is" | ">" | "(" | "of"))
    ((b ^^ { case b => !b }) <~ "neither") ~ repsep(p, sep("nor")) |
    (b <~ "either") ~ p ~ ("or" ~> p) ^^ {
      case b ~ p0 ~ p1 => new ~(b, List(p0, p1))
    } |
    (b <~ opt("either" | "one of")) ~ repsep(p <~ compoundGuard, sep("or")) |
    b ~ p ^^ { case b ~ p => new ~(b, List(p)) }
  private def isEither[T](p: Parser[T]): Parser[Boolean ~ List[T]] =
    either(isNeg, p)
  private def hasEither[T](p: Parser[T]): Parser[Boolean ~ List[T]] =
    either(hasNeg, p)
  private def isNeg: Parser[Boolean] =
    "is not" ^^^ true | "is" ^^^ false
  private def areNeg: Parser[Boolean] =
    ("are both not" | "are not") ^^^ true |
    ("are both" | "are") ^^^ false
  private def hasNeg: Parser[Boolean] =
    "does not have" ^^^ true | "has" ^^^ false

  // helper for creating expressions, conditions
  private def getRefExpr(r: Reference): Expression = ReferenceExpression(r)
  private def getExprCond(e: Expression): Condition = ExpressionCondition(e)

  // literal for mathematical one
  private val one = DecimalMathValueLiteral(1)
}
