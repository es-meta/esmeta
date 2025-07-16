package esmeta.lang.util

import esmeta.lang.*
import esmeta.ty.*
import esmeta.ty.util.{Parsers => TyParsers}
import esmeta.util.{IndentParsers, Locational, ConcreteLattice, ManualInfo}
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
    setStep |
    setAsStep |
    setEvalStateStep |
    performStep |
    invokeShorthandStep |
    returnStep |
    assertStep |
    // -------------------------------------------------------------------------
    throwStep |
    appendStep |
    prependStep |
    repeatStep |
    pushCtxtStep |
    noteStep |
    suspendStep |
    removeElemStep |
    removeFirstStep |
    removeCtxtStep |
    ifStep |
    forEachOwnPropertyKeyStep |
    forEachIntStep |
    forEachParseNodeStep |
    forEachStep |
    resumeStep |
    resumeYieldStep |
    blockStep |
    specialStep
  }.named("lang.Step")

  // let steps
  lazy val letStep: PL[LetStep] =
    ("let" ~> variable <~ "be") ~ endWithExpr ^^ { case x ~ e => LetStep(x, e) }

  // set steps
  lazy val setStep: PL[SetStep] =
    ("set" ~> ref) ~ ("to" ~> endWithExpr) ^^ { case r ~ e => SetStep(r, e) }

  // set-as steps
  lazy val setAsStep: PL[SetAsStep] =
    val verb = "specified" | "described"
    ("set" ~> ref) ~ ("as" ~> verb) ~ ("in" ~> xrefId <~ end) ^^ {
      case r ~ v ~ x => SetAsStep(r, v, x)
    }

  // set-eval-state steps
  lazy val setEvalStateStep: PL[SetEvaluationStateStep] =
    ("set the code evaluation state of " ~> ref <~
    "such that when evaluation is resumed for that execution context,") ~
    (variable <~ "will be called") ~ (argsPart <~ ".") ^^ {
      case c ~ f ~ a => SetEvaluationStateStep(c, f, a)
    }

  // perform steps
  lazy val performStep: PL[PerformStep] =
    "perform" ~> expr <~ end ^^ { PerformStep(_) }

  // invoke shorthand steps
  lazy val invokeShorthandStep: PL[InvokeShorthandStep] =
    opName ~ invokeArgs <~ end ^^ { case f ~ as => InvokeShorthandStep(f, as) }

  // return steps
  lazy val returnStep: PL[ReturnStep] =
    "return" ~> endWithExpr ^^ { ReturnStep(_) }

  // assertion steps
  lazy val assertStep: PL[AssertStep] =
    "assert" ~ ":" ~> (
      (upper ~> cond <~ end) |
      yetExpr ^^ { ExpressionCondition(_) }
    ) ^^ { AssertStep(_) }

  // ---------------------------------------------------------------------------
  // special steps rarely used in the spec
  // ---------------------------------------------------------------------------
  lazy val specialStep: PL[Step] = {
    setFieldsWithIntrinsicsStep |
    performBlockStep
  }.named("lang.Step")

  // set fields with intrinsics (CreateIntrinsics)
  lazy val setFieldsWithIntrinsicsStep: PL[SetFieldsWithIntrinsicsStep] = (
    "set fields of" ~> ref <~
      "with the values listed in" ~
      "<emu-xref href=\"#table-well-known-intrinsic-objects\"></emu-xref>."
  ) ~ ".*".r ^^ { case x ~ d => SetFieldsWithIntrinsicsStep(x, d) }

  // perform block steps (PerformEval)
  lazy val performBlockStep: PL[PerformBlockStep] =
    "perform the following substeps" ~
    "in an implementation-defined order" ~> opt("," ~> "[^:]*".r) ~
    (":" ~> stepBlock) ^^ { case d ~ b => PerformBlockStep(b, d.getOrElse("")) }

  // ---------------------------------------------------------------------------
  // TODO refactor following code
  // ---------------------------------------------------------------------------
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
    import MathOpExpressionOperator.Sub
    lazy val upper: Parser[Expression] =
      "≤" ~> calcExpr |
      "<" ~> calcExpr ^^ { e => MathOpExpression(Sub, List(e, one)) }
    lazy val interval: Parser[(Expression, Expression)] =
      ("starting with" ~> expr) ~ ("such that" ~ variable ~> (
        "≤" ^^^ { (x: CalcExpression) => x } |
        "<" ^^^ { BinaryExpression(_, BinaryExpressionOperator.Sub, one) }
      ) ~ calcExpr) ^^ {
        case l ~ (f ~ h) => (l, f(h))
      } | ("such that" ~> calcExpr <~ "≤" ~ variable) ~ upper ^^ {
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

  // for-each steps for OwnPropertyKey
  lazy val forEachOwnPropertyKeyStep: PL[ForEachOwnPropertyKeyStep] =
    import ForEachOwnPropertyKeyStepOrder.*
    lazy val ascending: Parser[Boolean] =
      ("ascending" ^^^ true | "descending" ^^^ false)
    lazy val order: Parser[ForEachOwnPropertyKeyStepOrder] =
      "numeric index order" ^^^ NumericIndexOrder |
      "chronological order of property creation" ^^^ ChronologicalOrder
    ("for each own property key" ~> variable) ~
    ("of" ~> variable <~ "such that") ~
    cond ~
    (", in" ~> ascending) ~
    order ~
    ("," ~ opt("do") ~> step) ^^ {
      case k ~ x ~ c ~ a ~ o ~ b => ForEachOwnPropertyKeyStep(k, x, c, a, o, b)
    }

  // for-each steps for parse node
  lazy val forEachParseNodeStep: PL[ForEachParseNodeStep] =
    ("for each child node" ~> variable) ~
    ("of" ~> expr <~ ",") ~ ("do" ~> step) ^^ {
      case x ~ e ~ body => ForEachParseNodeStep(x, e, body)
    }

  // throw steps
  lazy val throwStep: PL[ThrowStep] =
    lazy val errorName =
      "*" ~> word.filter(_.endsWith("Error")) <~ "*"
    "throw" ~> ("a" | "an") ~> errorName <~ "exception" <~ end ^^ {
      ThrowStep(_)
    }

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
    ("repeat" ~ ",") ~> opt("while" ~> cond <~ ",") ~ step ^^ {
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

  // remove element step
  lazy val removeElemStep: PL[RemoveStep] =
    "remove" ~> expr ~ ("from" ~> expr) <~ end ^^ {
      case e ~ l => RemoveStep(e, l)
    }

  // remove first element step
  lazy val removeFirstStep: PL[RemoveFirstStep] =
    "remove the first element from" ~> expr <~ end ^^ { RemoveFirstStep(_) }

  // remove execution context step
  lazy val removeCtxtStep: PL[RemoveContextStep] =
    val remove: Parser[Variable] =
      "remove" ~> variable <~ "from the execution context stack"
    val restore: Parser[Option[Variable]] = "and restore" ~> (
      variable ^^ { Some(_) } |
      "the execution context that is" ~
      "at the top of the execution context stack" ^^^ None
    ) <~ "as the running execution context"
    remove ~ restore <~ end ^^ { case x ~ y => RemoveContextStep(x, y) }

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

  // resume steps for yield
  lazy val resumeYieldStep: PL[ResumeYieldStep] =
    lazy val callerCtxt: P[Variable] = "Resume" ~> variable
    lazy val arg: P[Expression] = "passing" ~> expr <~ "."
    lazy val genCtxt: P[Variable] =
      "If" ~> variable <~ "is ever resumed again,"
    lazy val param: P[Variable] =
      "let" ~> variable <~ "be" ~
      "the Completion Record with which it is resumed." ~ guard("\n")
    callerCtxt ~ arg ~ genCtxt ~ param ~ rep1(subStep) ^^ {
      case c ~ e ~ r ~ v ~ subs => ResumeYieldStep(c, e, r, v, subs)
    }

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
    listCopyExpr |
    recordExpr |
    lengthExpr |
    substrExpr |
    trimExpr |
    numberOfExpr |
    sourceTextExpr |
    coveredByExpr |
    getItemsExpr |
    intrExpr |
    clampExpr |
    mathOpExpr |
    bitwiseExpr |
    listExpr |
    intListExpr |
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

  // list copy expressions
  lazy val listCopyExpr: PL[ListCopyExpression] =
    "a List whose elements are the elements of" ~> expr ^^ {
      ListCopyExpression(_)
    }

  // record expressions
  lazy val recordExpr: PL[RecordExpression] = {
    opt("the") ~> tname ~
    ("{" ~> repsep((fieldLiteral <~ ":") ~ expr, ",") <~ "}")
  } ^^ {
    case t ~ fs => RecordExpression(t, fs.map { case f ~ e => f -> e })
  } | {
    ("a new" ~> tname) ~
    ("whose" ~> fieldLiteral) ~
    ("is" ~> expr)
  } ^^ {
    case t ~ f ~ e => RecordExpression(t, List(f -> e))
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

  // trim expressions
  lazy val trimExpr: PL[TrimExpression] =
    ("the String value that is a copy of" ~> expr) ~
    ("with" ~> (
      "leading" ^^^ (true, false) |
      "trailing" ^^^ (false, true) |
      "both leading and trailing" ^^^ (true, true)
    ) <~ "white space removed") ^^ {
      case e ~ (l, t) => TrimExpression(e, l, t)
    }

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
      "the definition specified in" |
      "the algorithm steps defined in" |
      "the ordinary object internal method defined in"
    } ^^^ Algo | {
      "the internal slots listed in"
    } ^^^ InternalSlots | {
      "the number of non-optional parameters of" ~
      "the function definition in"
    } ^^^ ParamLength
    xrefOp ~ xrefId ^^ { case op ~ id => XRefExpression(op, id) }

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
      "abs" ^^^ Abs | "floor" ^^^ Floor |
      "truncate" ^^^ Truncate
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
    grammarSymbolLiteral |
    ntLiteral |
    "~" ~> "[-+a-zA-Z0-9]+".r <~ "~" ^^ { EnumLiteral(_) } |
    "the empty String" ^^! StringLiteral("") |
    strLiteral <~ opt("\\([^)]*\\)".r) |
    fieldLiteral |
    errObjLiteral |
    "%Symbol." ~> word <~ "%" ^^ { SymbolLiteral(_) } |
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
  // grammar symboll iterals
  lazy val grammarSymbolLiteral: PL[GrammarSymbolLiteral] =
    "the grammar symbol" ~ "|" ~> (word <~ opt("?")) ~ flags <~ "|" ^^ {
      case x ~ fs => GrammarSymbolLiteral(x, fs)
    }

  // nonterminal literals
  lazy val ntLiteral: PL[NonterminalLiteral] =
    opt("the") ~> opt(ordinal) ~ ("|" ~> word <~ opt("?")) ~ flags <~ "|" ^^ {
      case ord ~ x ~ fs => NonterminalLiteral(ord, x, fs)
    }

  lazy val flags: P[List[String]] =
    "[" ~> repsep("^[~+][A-Z][a-z]+".r, ",") <~ "]" | "" ^^^ Nil

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
    opt("the production") ~> tagged((word <~ ":") ~ ("[\\[\\]A-Za-z]+".r)) ^^ {
      case l ~ r => ProductionLiteral(l, r)
    }

  // error object literals
  lazy val errObjLiteral: PL[ErrorObjectLiteral] =
    lazy val errorName = "*" ~> word.filter(_.endsWith("Error")) <~ "*" ^^ {
      ErrorObjectLiteral(_)
    }
    "a newly created" ~> errorName <~ "object"

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
    // handle emu-meta tag
    tagged(opName) ~ invokeArgs ^^ {
      case x ~ as => InvokeAbstractOperationExpression(x, as)
    }

  // names for operations
  lazy val opName: Parser[String] =
    "[a-zA-Z][a-zA-Z0-9/]*".r.filter(!mathFuncNames.contains(_))
  lazy val mathFuncNames: Set[String] = Set(
    "max",
    "min",
    "abs",
    "floor",
    "truncate",
  )

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

  // integer list expressions
  lazy val intListExpr: PL[IntListExpression] =
    val inc = "(" ~> ("inclusive" ^^^ true | "exclusive" ^^^ false) <~ ")"
    val asc = "in" ~> ("ascending" ^^^ true | "descending" ^^^ false) <~ "order"
    "a List of the integers in the interval from" ~>
    (calcExpr ~ inc <~ "to") ~ (calcExpr ~ inc <~ ",") ~ asc ^^ {
      case (f ~ fi) ~ (t ~ ti) ~ a => IntListExpression(f, fi, t, ti, a)
    }

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
    "an instance of" ~> prodLiteral |
    // NumberBitwiseOp
    "the 32-bit two's complement bit string representing" ~> expr

  // not yet supported expressions
  lazy val yetExpr: PL[YetExpression] =
    ".+".r ~ opt(block) ^^ { case s ~ b => YetExpression(s, b) }

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

    compOr |||
    simpleImply |||
    simpleOr |||
    simpleAnd |||
    baseCond
  }.named("lang.Condition")

  // base conditions
  lazy val baseCond: PL[Condition] =
    specialCond |||
    containsCond |||
    inclusiveIntervalCond |||
    binCond |||
    isAreCond |||
    predCond |||
    productionCond |||
    hasBindingCond |||
    hasFieldCond |||
    typeCheckCond |||
    exprCond

  // expression conditions
  lazy val exprCond: PL[ExpressionCondition] = expr ^^ {
    ExpressionCondition(_)
  }

  // type check conditions
  lazy val typeCheckCond: PL[TypeCheckCondition] =
    expr ~ isEither(singleLangType) ^^ {
      case e ~ (n ~ t) => TypeCheckCondition(e, n, t)
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
    ("has" ^^^ false | "does not have" ^^^ true) ~
    ("a binding for" ~> expr) ^^ {
      case r ~ n ~ f => HasBindingCondition(r, n, f)
    }

  // production conditions
  // Ex: If _x_ is <emu-grammar>Statement : LabelledStatement</emu-grammar>, ...
  lazy val productionCond: PL[ProductionCondition] =
    (expr <~ "is" ~ opt("an instance of")) ~ prodLiteral ^^ {
      case nt ~ prod => ProductionCondition(nt, prod.lhs, prod.rhs) // TODO
    }

  // predicate conditions
  lazy val predCond: PL[PredicateCondition] =
    import PredicateConditionOperator.*
    lazy val op: Parser[PredicateConditionOperator] =
      "finite" ^^^ Finite |
      "a normal completion" ^^^ Normal |
      "an abrupt completion" ^^^ Abrupt |
      "a throw completion" ^^^ Throw |
      "a return completion" ^^^ Return |
      "a break completion" ^^^ Break |
      "a continue completion" ^^^ Continue |
      "never an abrupt completion" ^^^ NeverAbrupt |
      "duplicate entries" ^^^ Duplicated |
      "present" ^^^ Present |
      ("empty" | "an empty List") ^^^ Empty |
      "strict mode code" ^^^ StrictMode |
      "an array index" ^^^ ArrayIndex |
      "the token `false`" ^^^ FalseToken |
      "the token `true`" ^^^ TrueToken |
      "a data property" ^^^ DataProperty |
      "an accessor property" ^^^ AccessorProperty |
      "a fully populated Property Descriptor" ^^^ FullyPopulated |
      "an instance of a nonterminal" ^^^ Nonterminal

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
      "is the same sequence of code units as" ^^^ SameCodeUnits
    expr ~ op ~ expr ^^ { case l ~ o ~ r => BinaryCondition(l, o, r) }

  // inclusive interval conditions
  lazy val inclusiveIntervalCond: PL[InclusiveIntervalCondition] = {
    (expr <~ "is") ~
    opt("not") ~
    ("in the inclusive interval from" ~> expr) ~
    ("to" ~> expr)
  } ^^ {
    case l ~ n ~ f ~ t => InclusiveIntervalCondition(l, n.isDefined, f, t)
  }

  // `contains` conditions
  lazy val containsCond: PL[ContainsCondition] = {
    expr ~
    ("does not contain" ^^^ true | "contains" ^^^ false) ~
    containsTarget
  } ^^ { case l ~ n ~ e => ContainsCondition(l, n, e) }
  lazy val containsTarget: P[ContainsConditionTarget] =
    import ContainsConditionTarget.*
    lazy val exprTarget = expr ^^ { Expr(_) }
    lazy val targetType = "an element" ^^^ None | langType ^^ Some.apply
    lazy val whoseFieldTarget = {
      (targetType <~ "whose") ~
      ("[[" ~> word <~ "]]") ~
      ("is" ~> expr)
    } ^^ { case t ~ f ~ e => WhoseField(t, f, e) }
    lazy val suchThatTarget = {
      targetType ~
      variable ~
      ("such that" ~> cond)
    } ^^ { case t ~ x ~ c => SuchThat(t, x, c) }
    suchThatTarget |||
    whoseFieldTarget |||
    exprTarget

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
    ("there does not exist an element" ~> variable) ~
    ("of" ~> expr) ~
    ("such that" ~> cond)
  } ^^ {
    case x ~ l ~ c =>
      ContainsCondition(l, true, ContainsConditionTarget.SuchThat(None, x, c))
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
  } | {
    // InitializeHostDefinedRealm
    "the host requires use of an exotic object to serve as _realm_'s global object" |
    "the host requires that the `this` binding in _realm_'s global scope return an object other than the global object"
  } ^^! getExprCond(
    FalseLiteral(),
  ) | {
    // PropertyDefinitionEvaluation
    // NOTE if JSON.parse is supported, then the next line should be handled properly
    "this |PropertyDefinition| is contained within a |Script| that is being evaluated for ParseJSON (see step <emu-xref href=\"#step-json-parse-eval\"></emu-xref> of ParseJSON)"
  } ^^! getExprCond(FalseLiteral())

  // ---------------------------------------------------------------------------
  // metalanguage references
  // ---------------------------------------------------------------------------
  given ref: PL[Reference] = {
    specialRef |||
    propRef |||
    baseRef
  }.named("lang.Reference")

  // property references
  lazy val propRef: PL[PropertyReference] = opt(
    "the" ~ opt("String") ~ "value" ~ opt("of"),
  ) ~> {
    prop ~ baseRef ^^ {
      case p ~ base => PropertyReference(base, p)
    } ||| baseRef ~ prop ~ rep(prop) ^^ {
      case base ~ p ~ ps =>
        ps.foldLeft(PropertyReference(base, p))(PropertyReference(_, _))
    }
  }

  // base references
  lazy val baseRef: PL[Reference] =
    opt(nt) ~> variable |
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
    ("the" ~> ordinal <~ "element") ~ (("of" | "from") ~> ref)
  } ^^ {
    case o ~ b =>
      PropertyReference(b, IndexProperty(DecimalMathValueLiteral(o - 1)))
  } | {
    // SetFunctionName, SymbolDescriptiveString
    (variable <~ "'s") ~ ("[[" ~> word <~ "]]") <~ "value"
  } ^^ { case b ~ f => PropertyReference(b, FieldProperty(f)) } | {
    // AgentSignifier or AgentCanSuspend
    "the Agent Record of the surrounding agent" ^^! AgentRecord()
  }

  // ---------------------------------------------------------------------------
  // metalanguage properties
  // ---------------------------------------------------------------------------
  given prop: PL[Property] = preProp | postProp
  lazy val preProp: PL[Property] = {
    "the" ~> nt <~ "of" ^^ { NonterminalProperty(_) } |||
    "the binding for" ~> expr <~ "in" ^^ { BindingProperty(_) } |||
    "the" ~> component <~ opt("component") ~ "of" ^^ { ComponentProperty(_) }
  }.named("lang.Property")

  lazy val postProp: PL[Property] = {
    "[" ~> expr <~ "]" ^^ { IndexProperty(_) } |||
    ("'s" | ".") ~> camel.filter(_ != "If") ^^ { ComponentProperty(_) } |||
    "." ~ "[[" ~> intr <~ "]]" ^^ { i => IntrinsicProperty(i) } |||
    "." ~> "[[" ~> word <~ "]]" ^^ { FieldProperty(_) }
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
    opt("the intrinsic function") ~ "%" ~> not("Symbol.") ~> (word ~ rep(
      "." ~> word,
    )) <~ "%" ^^ {
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
  lazy val singlePureValueTy: P[ValueTy] =
    (listTy | cloTy | astTy | grammarSymbolTy | simpleTy) ||| recordTy

  // named record types
  lazy val recordTy: P[ValueTy] =
    "Record" ~ "{" ~> repsep(fieldLiteral, ",") <~ "}" ^^ {
      case fs => RecordT("", fs.map(_.name -> AnyT).toMap)
    } | opt("an " | "a ") ~> {
      "function object" ^^^ FunctionT |
      "constructor" ^^^ ConstructorT | (
        "ordinary object" |
        "ECMAScript function object" |
        "built-in function object" |
        "Array exotic object" ^^^ "Array" |
        "arguments exotic object" |
        "String exotic object" |
        "Proxy exotic object" |
        "bound function exotic object" |
        "immutable prototype exotic object" |
        "module namespace exotic object" |
        "mutable binding" |
        "execution context" |
        "Module Namespace Object" ^^^ "ModuleNamespaceExoticObject" |
        "error"
      ) ^^ { normRecordT(_) } |||
      rep1(camel) ^^ { case ss => normRecordT(ss.mkString(" ")) }
    } <~ opt("s")

  // list types
  lazy val listTy: P[ValueTy] =
    opt("an " | "a ") ~ "List of" ~> pureValueTy ^^ { ListT(_) }

  // closure types
  // TODO more details
  lazy val cloTy: P[ValueTy] =
    "an" ~ "Abstract Closure" ~ "with" ~>
    ("no" ~ "parameters") ^^ { case _ => CloT }

  // AST types
  lazy val astTy: P[ValueTy] =
    val singleAstTy = article ~> nt <~ opt("Parse Node")
    article ~ "Parse Node" ~ opt("s") ^^^ AstT |
    rep1sep(singleAstTy, sep("or")) ^^ { ss => AstT(ss.toSet) }

  // grammar symbol types
  lazy val grammarSymbolTy: P[ValueTy] = "a grammar symbol" ^^^ GrammarSymbolT

  // simple types
  lazy val simpleTy: P[ValueTy] = opt("an " | "a ") ~> {
    "Number" ^^^ NumberT |
    "BigInt" ^^^ BigIntT |
    "Boolean" ^^^ BoolT |
    "String" ~ opt("which is[^,]*".r) ^^^ StrT |
    "*undefined*" ^^^ UndefT |
    "*null*" ^^^ NullT |
    "*false*" ^^^ FalseT |
    "*true*" ^^^ TrueT |
    "integer" ^^^ IntT |
    "non-negative integer" ^^^ NonNegIntT |
    // TODO See https://tc39.es/ecma262/2024/#sec-typedarray-objects
    // "TypedArray element type" ^^^ EnumT(
    //   "int8",
    //   "uint8",
    //   "uint8clamped",
    //   "int16",
    //   "uint16",
    //   "int32",
    //   "uint32",
    //   "bigint64",
    //   "biguint64",
    //   "float32",
    //   "float64",
    // ) |
    "negative integer" ^^^ NegIntT |
    "non-positive integer" ^^^ NonPosIntT |
    "positive integer" ^^^ PosIntT |
    "positive number" ^^^ PosNumberT |
    "non-negative number" ^^^ NonNegNumberT |
    "non-positive number" ^^^ NonPosNumberT |
    "negative number" ^^^ NegNumberT |
    decimal ^^ { MathT(_) } |
    "+∞" ^^^ PosInfinityT |
    "-∞" ^^^ NegInfinityT |
    "ECMAScript language value" ^^^ ESValueT |
    "internal slot name" ^^^ StrT |
    "Array" ^^^ ArrayT |
    "TypedArray" ^^^ TypedArrayT |
    opt("initialized") ~ "RegExp" ~ opt("instance") ^^^ RegExpT |
    "non-negative integral Number" ^^^ NumberNonNegIntT |
    "*NaN*" ^^! NaNT |
    "integral Number" ^^^ NumberIntT |
    "property key" ^^^ (StrT || SymbolT) |
    "~" ~> "[-+a-zA-Z0-9]+".r <~ "~" ^^ { EnumT(_) }
  } <~ opt("s")

  // rarely used expressions
  lazy val specialTy: P[Ty] = opt("an " | "a ") ~> {
    "List of" ~> word ^^ {
      case s => UnknownTy(s"List of $s")
    } | (nt | tname) ^^ {
      case s => UnknownTy(s)
    }
  }

  // type name
  lazy val tname: P[String] = {
    opt("ECMAScript code") ~ "execution context" ^^^ "ExecutionContext" |
    "\\w+ Environment Record".r |
    "[a-zA-Z ]+ object".r
  } ||| rep1(camel) ^^ { case ss => ss.mkString(" ") }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private def normRecordT(s: String): ValueTy = RecordT(Type.normalizeName(s))

  private def multi(parser: P[ValueTy], either: Boolean = true): P[ValueTy] =
    val multiParser = (if (either) "either" else "") ~> {
      rep1sep(parser, ",") ~ (sep("or") ~> parser)
    } ^^ { case ts ~ t => ts.foldLeft(t)(_ || _) }
    multiParser | parser

  // html tags
  case class Tagged[T](content: T, tag: String, fields: Map[String, String])
  private def tagged[T](parser: Parser[T]): Parser[T] =
    val tagStart: Parser[String] = "<[^>]+>".r
    val tagEnd: Parser[String] = "</[a-z-]+>".r
    opt(tagStart) ~> parser <~ opt(tagEnd)
  private def withTag[T](parser: Parser[T]): Parser[Tagged[T]] =
    val name = "[a-z-]+".r
    val str = "\"[^\"]*\"".r
    val fields = rep(name ~ opt("=" ~> str)) ^^ {
      _.map { case f ~ v => f -> v.fold("")(_.drop(1).dropRight(1)) }.toMap
    }
    ("<" ~> name) ~ (fields <~ ">") ~ parser ~ ("</" ~> name <~ ">") ^? {
      case l ~ fs ~ c ~ r if l == r => Tagged(c, l, fs)
    }

  lazy val xrefId: P[String] = withTag("") ^? {
    case Tagged("", "emu-xref", fs)
        if fs.get("href").exists(_.startsWith("#")) =>
      fs("href").drop(1)
  }

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

  // arguments part
  lazy val argsPart = "with" ~> (
    "no arguments" ^^^ Nil |
    ("arguments" | "argument") ~> repsep(expr, sep("and"))
  )

  // helper for creating expressions, conditions
  private def getRefExpr(r: Reference): Expression = ReferenceExpression(r)
  private def getExprCond(e: Expression): Condition = ExpressionCondition(e)

  // literal for mathematical one
  private val one = DecimalMathValueLiteral(1)

  // article
  private val article = opt("a " | "an " | "the ")
}
