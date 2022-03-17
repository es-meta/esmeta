package esmeta.lang.util

import esmeta.lang.*
import esmeta.util.{IndentParsers, Locational}
import scala.util.matching.Regex

/** language parser */
object Parser extends Parsers
trait Parsers extends IndentParsers {

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

  // sub-steps
  lazy val subStepPrefix: Parser[Option[String]] =
    val directive =
      ("[id=\"" ~> "[-a-zA-Z0-9]+".r <~ "\"]") |
      ("[fence-effects=\"" ~> "[-a-zA-Z0-9]+".r <~ "\"]")
    next ~> "1." ~> opt(directive) <~ upper
  lazy val subStep: Parser[SubStep] =
    subStepPrefix ~ (step <~ guard(EOL) | yetStep) ^^ {
      case x ~ s => SubStep(x, s)
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
    performStep |
    performBlockstep |
    returnToResumedStep |
    returnStep |
    assertStep |
    throwStep |
    appendStep |
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
    ("let" ~> variable <~ "be") ~ endWithExpr ^^ { case x ~ e => LetStep(x, e) }

  // set steps
  lazy val setStep: PL[SetStep] =
    ("set" ~> ref <~ ("as" | "to")) ~ endWithExpr ^^ {
      case r ~ e => SetStep(r, e)
    }

  // if-then-else steps
  lazy val ifStep: PL[IfStep] =
    ("if" ~> cond <~ "," ~ opt("then")) ~ step ~ opt(
      opt(subStepPrefix) ~ ("else" | "otherwise") ~ opt(",") ~> step,
    ) ^^ { case c ~ t ~ e => IfStep(c, t, e) }

  // return steps
  lazy val returnStep: PL[ReturnStep] =
    "return" ~> end ^^! { ReturnStep(None) } |
    "return" ~> endWithExpr ^^ { case e => ReturnStep(Some(e)) }

  // assertion steps
  lazy val assertStep: PL[AssertStep] =
    "assert" ~ ":" ~> (upper ~> cond) <~ end ^^ { AssertStep(_) }

  // for-each steps
  lazy val forEachStep: PL[ForEachStep] =
    lazy val ascending: Parser[Boolean] =
      opt("in reverse List order,") ^^ { !_.isDefined }
    ("for each" ~ opt("element") ~> opt(ty)) ~ variable ~
    ("of" ~> expr) ~ ("," ~> ascending) ~ (opt("do") ~> step) ^^ {
      case t ~ r ~ e ~ a ~ s =>
        ForEachStep(t, r, e, a, s)
    }

  // for-each steps for integers
  lazy val forEachIntStep: PL[ForEachIntegerStep] =
    lazy val ascending: Parser[Boolean] =
      ("ascending" ^^! true | "descending" ^^! false)
    ("for each" ~ "(non-negative )?integer".r ~> variable) ~
    ("starting with" ~> expr) ~
    ("such that" ~> cond) ~
    (", in" ~> ascending <~ "order,") ~
    (opt("do") ~> step) ^^ {
      case x ~ start ~ cond ~ asc ~ body =>
        ForEachIntegerStep(x, start, cond, asc, body)
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
    "throw a *" ~> word <~ "* exception" <~ end ^^ { ThrowStep(_) }

  // perform steps
  lazy val performStep: PL[PerformStep] =
    opt("perform" | "call") ~> (invokeExpr | returnIfAbruptExpr) <~ end ^^ {
      PerformStep(_)
    }

  // peform block steps
  lazy val performBlockstep: PL[PerformBlockStep] =
    "perform the following substeps in an implementation-defined order" ~ ".*".r ~> stepBlock ^^ {
      PerformBlockStep(_)
    }

  // append steps
  lazy val appendStep: PL[AppendStep] =
    ("append" | "add") ~> expr ~
    ((("to" ~ opt("the end of")) | "as the last element of") ~> ref) <~ end
    ^^ { case e ~ r => AppendStep(e, r) }

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
      "for that execution context" ^^^ { None } |
      "with a" ~ opt(ty) ~> variable ^^ { Some(_) } // TODO handle type
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
      tagStart ~ "Resume the suspended evaluation of" ~> variable <~ tagEnd
    lazy val arg: P[Option[Expression]] =
      "using" ~> expr <~ "as the result of the operation that suspended it." ^^ {
        Some(_)
      } | "." ^^^ { None }
    lazy val param: P[Option[Variable]] =
      "Let" ~> variable <~ "be the" ~ ("value" | "completion record") ~ "returned by the resumed computation." <~ guard(
        "\n",
      ) ^^ { Some(_) } | guard("\n") ^^^ { None }
    context ~ arg ~ param ~ rep1(subStep) ^^ {
      case c ~ a ~ p ~ subs => ResumeEvaluationStep(c, a, p, subs)
    }

  // return to resumed steps
  lazy val returnToResumedStep: PL[ReturnToResumeStep] =
    val context: P[Variable] =
      next ~ "1. NOTE: This returns to the evaluation of the operation that had most previously resumed evaluation of" ~> variable <~ "."
    returnStep ~ context ^^ { case a ~ c => ReturnToResumeStep(c, a) }

  // block steps
  lazy val blockStep: PL[BlockStep] = stepBlock ^^ { BlockStep(_) }

  // not yet supported steps
  lazy val yetStep: PL[YetStep] = yetExpr ^^ { YetStep(_) }

  // end of step
  lazy val note = "NOTE:" ~> ".*".r
  lazy val ignore =
    "(" ~ "see.*\\)".r | "as defined in" ~ tagStart ~ tagEnd | "; that is[^.]*".r
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
    stringConcatExpr |||
    listConcatExpr |||
    recordExpr |||
    lengthExpr |||
    substrExpr |||
    numberOfExpr |||
    sourceTextExpr |||
    coveredByExpr |||
    getChildrenExpr |||
    intrExpr |||
    calcExpr |||
    invokeExpr |||
    returnIfAbruptExpr |||
    listExpr |||
    xrefExpr |||
    soleExpr |||
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
  lazy val recordExpr: PL[RecordExpression] =
    opt("the") ~> ty ~
    ("{" ~> repsep((fieldLiteral <~ ":") ~ expr, ",") <~ "}") ^^ {
      case t ~ fs =>
        val fields = fs.map { case f ~ e => f -> e }
        RecordExpression(t, fields)
    } |||
    opt("an" | "a") ~ ("newly created" | "new") ~
    guard(not("Realm")) ~> ty <~ opt(
      "containing no bindings" |
      "with no fields" |
      "that initially has no fields",
    ) ^^ { case t => RecordExpression(t, List()) }

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
    ("to" ~> expr) ^^ { case e ~ f ~ t => SubstringExpression(e, f, t) }

  // `the number of elements in` expressions
  lazy val numberOfExpr: PL[NumberOfExpression] =
    ("the number of elements in" ~ opt("the List") ~> expr) ^^ {
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

  // get children ast expressions
  lazy val getChildrenExpr: PL[GetChildrenExpression] =
    ("the List of" ~> expr <~ "items") ~
    ("in" ~> expr <~ "," ~ "in source text order") ^^ {
      case t ~ e => GetChildrenExpression(t, e)
    }

  // abstract closure expressions
  lazy val closureExpr: PL[AbstractClosureExpression] =
    lazy val params: P[List[Variable]] =
      "no parameters" ^^! { Nil } |
      "parameters" ~> ("(" ~> repsep(variable, ",") <~ ")")
    lazy val captured: P[List[Variable]] =
      "captures nothing" ^^! { Nil } |
      "captures" ~> repsep(variable, sep("and"))

    "a new" ~ opt(
      "Job",
    ) ~ "Abstract Closure with" ~> params ~ ("that" ~> captured) ~
    ("and performs the following steps when called:" ~> blockStep) ^^ {
      case ps ~ cs ~ body => AbstractClosureExpression(ps, cs, body)
    }

  // intrinsic expressions
  lazy val intrExpr: PL[IntrinsicExpression] = intr ^^ {
    IntrinsicExpression(_)
  }

  // calculation expressions
  lazy val calcExpr: PL[CalcExpression] = {
    import BinaryExpression.Op.*
    import UnaryExpression.Op.*

    lazy val base: PL[CalcExpression] =
      refExpr ||| literal ||| mathOpExpr ||| returnIfAbruptExpr ||| "(" ~> calc <~ ")" ||| (
        base ~ ("<sup>" ~> calc <~ "</sup>")
      ) ^^ { case b ~ e => ExponentiationExpression(b, e) }

    lazy val unary: PL[CalcExpression] = base ||| (
      ("-" | "the result of negating") ^^! Neg
    ) ~ base ^^ {
      case o ~ e => UnaryExpression(o, e)
    }

    lazy val term: PL[CalcExpression] = unary ~ rep(
      ("×" ^^! Mul ||| "/" ^^! Div ||| "modulo" ^^! Mod) ~ unary,
    ) ^^ {
      case l ~ rs =>
        rs.foldLeft(l) { case (l, op ~ r) => BinaryExpression(l, op, r) }
    }

    lazy val calc: PL[CalcExpression] = term ~ rep(
      ("+" ^^! Add ||| "-" ^^! Sub) ~ term,
    ) ^^ {
      case l ~ rs =>
        rs.foldLeft(l) { case (l, op ~ r) => BinaryExpression(l, op, r) }
    }

    calc
  }

  // emu-xref expressions
  // TODO cleanup spec.html
  lazy val xrefExpr: PL[XRefExpression] =
    import XRefExpression.Op.*
    lazy val xrefOp: P[XRefExpression.Op] =
      ("specified in" | "described in" | "the definition specified in" | "the algorithm steps defined in") ^^! {
        Algo
      } |
      "the internal slots listed in" ^^! { InternalSlots } |
      "the number of non-optional parameters of the function definition in" ^^! {
        ParamLength
      }

    xrefOp ~ ("<emu-xref href=\"#" ~> "[a-z-.]+".r <~ "\"[a-z ]*>".r ~ tagEnd) ^^ {
      case op ~ id => XRefExpression(op, id)
    }

  // the sole element expressions
  lazy val soleExpr: PL[SoleElementExpression] =
    ("the sole element of" | "the string that is the only element of")
    ~> expr ^^ { SoleElementExpression(_) }

  // reference expressions
  lazy val refExpr: PL[ReferenceExpression] = ref ^^ { ReferenceExpression(_) }

  // mathematical operation expressions
  lazy val mathOpExpr: PL[MathOpExpression] =
    import MathOpExpression.Op.*
    (
      "max" ^^! Max ||| "min" ^^! Min |||
      "abs" ^^! Abs ||| "floor" ^^! Floor |||
      "ℤ" ^^! ToBigInt ||| "𝔽" ^^! ToNumber ||| "ℝ" ^^! ToMath
    ) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case o ~ as =>
        MathOpExpression(o, as)
    }

  // literals
  // GetIdentifierReference uses 'the value'
  lazy val literal: PL[Literal] = opt("the" ~ opt(ty) ~ "value") ~> (
    (opt("the") ~> "*this* value" | "this Parse Node") ^^! ThisLiteral() |||
    "NewTarget" ^^! NewTargetLiteral() |||
    hexLiteral |||
    "`[^`]+`".r ^^ { case s => CodeLiteral(s.substring(1, s.length - 1)) } |||
    ntLiteral |||
    "~" ~> "[-+a-zA-Z0-9]+".r <~ "~" ^^ { ConstLiteral(_) } |||
    "the empty String" ^^! StringLiteral("") |||
    strLiteral |||
    fieldLiteral |||
    "@@" ~> word ^^ { SymbolLiteral(_) } |||
    "+∞" ^^! PositiveInfinityMathValueLiteral() |||
    "-∞" ^^! NegativeInfinityMathValueLiteral() |||
    number ^^ { case s => DecimalMathValueLiteral(BigDecimal.exact(s)) } |||
    "*+∞*<sub>𝔽</sub>" ^^! NumberLiteral(Double.PositiveInfinity) |||
    "*-∞*<sub>𝔽</sub>" ^^! NumberLiteral(Double.NegativeInfinity) |||
    "*NaN*" ^^! { NumberLiteral(Double.NaN) } |||
    "*" ~> double <~ "*<sub>𝔽</sub>" ^^ { NumberLiteral(_) } |||
    "*" ~> bigint <~ "*<sub>ℤ</sub>" ^^ { BigIntLiteral(_) } |||
    "*true*" ^^! TrueLiteral() |||
    "*false*" ^^! FalseLiteral() |||
    "*undefined*" ^^! UndefinedLiteral() |||
    "*null*" ^^! NullLiteral() |||
    "absent" ^^! AbsentLiteral() |||
    "Undefined" ^^! UndefinedTypeLiteral() |||
    "Null" ^^! NullTypeLiteral() |||
    "Boolean" ^^! BooleanTypeLiteral() |||
    "String" ^^! StringTypeLiteral() |||
    "Symbol" ^^! SymbolTypeLiteral() |||
    "Number" ^^! NumberTypeLiteral() |||
    "BigInt" ^^! BigIntTypeLiteral() |||
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
      "[" ~> repsep("^[~+][A-Z][a-z]+".r, ",") <~ "]" | "" ^^^ List()
    opt("the grammar symbol" | "the" | "this") ~> opt(ordinal) ~
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
    (tagStart ~> word <~ ":") ~ ("[\\[\\]A-Za-z]+".r <~ tagEnd) ^^ {
      case l ~ r => ProductionLiteral(l, r)
    }

  // metalanguage invocation expressions
  lazy val invokeExpr: PL[InvokeExpression] =
    invokeAOExpr |||
    invokeNumericExpr |||
    invokeClosureExpr |||
    invokeAMExpr |||
    invokeSDOExpr

  // arguments for invocation epxressions
  lazy val invokeArgs: P[List[Expression]] = ("(" ~> repsep(expr, ",") <~ ")")

  // abstract operation (AO) invocation expressions
  lazy val invokeAOExpr: PL[InvokeAbstractOperationExpression] =
    opt(tagStart) ~> "(this)?[A-Z][a-zA-Z0-9/]*".r ~ invokeArgs <~ opt(
      tagEnd,
    ) ^^ {
      case x ~ as => InvokeAbstractOperationExpression(x, as)
    }

  // numeric method invocation expression
  lazy val invokeNumericExpr: PL[InvokeNumericMethodExpression] =
    guard(not("Return")) ~> ty ~ ("::" ~> "[A-Za-z]+".r) ~ invokeArgs ^^ {
      case t ~ op ~ as =>
        InvokeNumericMethodExpression(t, op, as)
    }

  // abstract closure invocation expression
  lazy val invokeClosureExpr: PL[InvokeAbstractClosureExpression] =
    variable ~ invokeArgs ^^ {
      case v ~ as =>
        InvokeAbstractClosureExpression(v, as)
    }

  // method invocation expressions
  lazy val invokeAMExpr: PL[InvokeMethodExpression] =
    // handle emu-meta tag
    (opt(tagStart) ~> propRef <~ opt(tagEnd)) ~ (invokeArgs <~ opt(tagEnd)) ^^ {
      case p ~ as => InvokeMethodExpression(p, as)
    }

  // syntax-directed operation (SDO) invocation expressions
  lazy val invokeSDOExpr: PL[InvokeSyntaxDirectedOperationExpression] =
    lazy val name =
      (opt("the result of performing" | "the result of" | "the") ~ guard(
        not(component),
      ) ~> camel)
    lazy val base = ("of" ~> expr)
    lazy val args = repsep(expr, sep("and"))
    lazy val argsPart = (
      ("using" | "with" | "passing") ~> args <~
        "as" ~ opt("the") ~ ("arguments" | "argument") |||
        "with" ~ ("arguments" | "argument") ~> args
    )

    // normal SDO
    lazy val normalSDOExpr =
      name ~ base ~ opt(argsPart) ^^ {
        case x ~ b ~ as =>
          InvokeSyntaxDirectedOperationExpression(b, x, as.getOrElse(Nil))
      }

    // Evalution SDO
    lazy val evalSDOExpr =
      "the result of evaluating" ~ opt(ty <~ guard(expr)) ~> expr ^^ {
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
    ("?" ^^! true | "!" ^^! false) ~ invokeExpr ^^ {
      case c ~ e => ReturnIfAbruptExpression(e, c)
    }

  // list expressions
  lazy val listExpr: PL[ListExpression] =
    "a new empty List" ^^! ListExpression(Nil) |
    "«" ~> repsep(expr, ",") <~ "»" ^^ { ListExpression(_) } |
    "a List whose sole element is" ~> expr ^^ { e => ListExpression(List(e)) }

  // rarely used expressions
  lazy val specialExpr: PL[Expression] =
    // ClassStaticBlockDefinitionEvaluation
    "the empty sequence of Unicode code points" ^^! StringLiteral("") |
    // Array.prototype.join
    "the single-element String" ~> strLiteral |
    // CreateDynamicFunction
    strLiteral <~ "\\([^)]*\\)".r |
    // MethodDefinitionEvaluation, ClassFieldDefinitionEvaluation
    "an instance of the production" ~> prodLiteral

  // not yet supported expressions
  lazy val yetExpr: PL[YetExpression] =
    opt("[YET]") ~> ".+".r ~ opt(block) ^^ { case s ~ b => YetExpression(s, b) }

  // ---------------------------------------------------------------------------
  // metalanguage conditions
  // ---------------------------------------------------------------------------
  given cond: PL[Condition] = {
    import CompoundCondition.Op.*

    // get compound condition from base and operation
    def compound(
      base: P[Condition],
      op: Parser[CompoundCondition.Op],
    ): Parser[Condition] =
      rep(base <~ opt(",")) ~ op ~ (opt("if") ~> base) ^^ {
        case ls ~ op ~ r =>
          ls.foldRight(r) {
            case (l, r) => CompoundCondition(l, op, r)
          }
      }

    lazy val simpleAnd: P[Condition] = compound(baseCond, "and" ^^! And)
    lazy val simpleOr: P[Condition] = compound(baseCond, "or" ^^! Or)
    lazy val simpleImply: P[Condition] =
      "If" ~> compound(baseCond, "then" ^^! Imply)
    lazy val compOr: P[Condition] = compound(simpleAnd, "or" ^^! Or)

    baseCond ||| simpleAnd ||| simpleOr ||| simpleImply ||| compOr
  }.named("lang.Condition")

  // base conditions
  lazy val baseCond: PL[Condition] =
    exprCond |||
    instanceOfCond |||
    hasFieldCond |||
    productionCond |||
    predCond |||
    isAreCond |||
    binCond |||
    specialCond

  // expression conditions
  lazy val exprCond: PL[ExpressionCondition] = expr ^^ {
    ExpressionCondition(_)
  }

  // instance check conditions
  lazy val instanceOfCond: PL[InstanceOfCondition] =
    expr ~ isEither((("an" | "a") ~> ty)) ^^ {
      case e ~ (n ~ t) => InstanceOfCondition(e, n, t)
    }

  // field includsion conditions
  lazy val hasFieldCond: PL[HasFieldCondition] =
    lazy val fieldStr = "field" | ("internal" ~ ("method" | "slot"))
    // GeneratorValidate
    (ref <~ opt("also")) ~
    ("has" ^^! false ||| "does not have" ^^! true) ~
    (("an" | "a") ~> expr <~ fieldStr) ^^ {
      case r ~ n ~ f => HasFieldCondition(r, n, f)
    }

  // production conditions
  // Ex: If _x_ is <emu-grammar>Statement : LabelledStatement</emu-grammar>, ...
  lazy val productionCond: PL[ProductionCondition] =
    (expr <~ "is") ~ prodLiteral ^^ {
      case nt ~ prod => ProductionCondition(nt, prod.lhs, prod.rhs) // TODO
    }

  // predicate conditions
  lazy val predCond: PL[PredicateCondition] =
    import PredicateCondition.Op.*
    lazy val op: Parser[PredicateCondition.Op] =
      "finite" ^^^ { Finite } |
      "an abrupt completion" ^^^ { Abrupt } |
      ("a normal completion" | "never an abrupt completion") ^^^ { Normal } |
      "duplicate entries" ^^^ { Duplicated } |
      "present" ^^^ { Present } |
      ("empty" | "an empty List") ^^^ { Empty } |
      "strict mode code" ^^^ { StrictMode } |
      "an array index" ^^^ { ArrayIndex } |
      "a non-negative integral Number" ^^^ { NonNegative } |
      "the token `false`" ^^^ { FalseToken } |
      "the token `true`" ^^^ { TrueToken } |
      "a data property" ^^^ { DataProperty } |
      "an accessor property" ^^^ { AccessorProperty } |
      "a fully populated Property Descriptor" ^^^ { FullyPopulated } |
      "an instance of a nonterminal" ^^^ { Nonterminal }

    lazy val neg: Parser[Boolean] =
      isNeg | ("contains" | "has") ~> ("any" ^^^ { false } | "no" ^^^ { true })

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
    import BinaryCondition.Op.*
    lazy val op: Parser[BinaryCondition.Op] =
      "=" ^^! Eq |||
      "≠" ^^! NEq |||
      "<" ^^! LessThan |||
      "≤" ^^! LessThanEqual |||
      ">" ^^! GreaterThan |||
      "≥" ^^! GreaterThanEqual |||
      "is the same sequence of code units as" ^^! SameCodeUnits |||
      "contains" ^^! Contains |||
      "does not contain" ^^! NContains
    expr ~ op ~ expr ^^ { case l ~ o ~ r => BinaryCondition(l, o, r) } |||
    expr ~ (isNeg <~ (opt("currently") ~> "an element of")) ~ expr ^^ {
      case l ~ n ~ r =>
        BinaryCondition(r, if (n) NContains else Contains, l)
    }

  // rarely used conditions
  // TODO clean-up
  lazy val specialCond: PL[Condition] =
    // ResolveBinding
    "the source text matched by the syntactic production that is being evaluated is contained in strict mode code" ^^! {
      getExprCond(TrueLiteral())
    } |
    // Script.IsStrict
    "the Directive Prologue of |ScriptBody| contains a Use Strict Directive" ^^! {
      getExprCond(TrueLiteral()) // assume strict
    } |
    // PropertyDefinition[2,0].PropertyDefinitionEvaluation
    "this |PropertyDefinition| is contained within a |Script| that is being evaluated for JSON.parse" ~
    ignore ~ guard(",") ^^! {
      getExprCond(FalseLiteral())
    } |
    // CreatePerIterationEnvironment
    expr <~ "has any elements" ^^ {
      case r => PredicateCondition(r, true, PredicateCondition.Op.Empty)
    } |
    // ForBodyEvaluation
    expr ~ isNeg <~ "~[empty]~" ^^ {
      case e ~ n => PredicateCondition(e, !n, PredicateCondition.Op.Present)
    } |
    // %ForInIteratorPrototype%.next
    ("there does not exist an element" ~ variable ~ "of" ~> variable) ~
    ("such that SameValue(" ~> variable <~ "," ~ variable ~ ") is *true*") ^^ {
      case list ~ elem =>
        BinaryCondition(
          getRefExpr(list),
          BinaryCondition.Op.NContains,
          getRefExpr(elem),
        )
    } |
    // CallExpression[0,0].Evaluation
    expr <~ "has no elements" ^^ {
      case r => PredicateCondition(r, false, PredicateCondition.Op.Empty)
    } |
    // ArraySpeciesCreate, SameValueNonNumeric
    expr ~ ("and" ~> expr) ~ areNeg <~ "the same" ~ opt(ty) ~ opt("value") ^^ {
      case l ~ r ~ n => IsAreCondition(List(l), n, List(r))
    } |
    // SameValueNonNumeric, GeneratorValidate
    expr ~ (isNeg <~ ("the same" ~ opt(ty) ~ opt("value") ~ "as")) ~ expr ^^ {
      case l ~ n ~ r => IsAreCondition(List(l), n, List(r))
    } |
    // SameValue
    expr ~ (isNeg <~ "different from" ^^ { !_ }) ~ expr ^^ {
      case l ~ n ~ r => IsAreCondition(List(l), n, List(r))
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
    } |
    "the current Realm Record" ^^! { CurrentRealmRecord() } |
    ("the active function object" | "the active function") ^^! {
      ActiveFunctionObject()
    } |
    "the second to top element of the execution context stack" ^^! {
      SecondExecutionContext()
    }

  // variables
  lazy val variable: PL[Variable] = "_[^_]+_".r ^^ {
    case s => Variable(s.substring(1, s.length - 1))
  }

  // special reference
  lazy val specialRef: P[Reference] =
    // IsLessThan
    "the" ~> variable <~ "flag" |
    // GetPrototypeFromConstructor
    (variable <~ "'s intrinsic object named") ~ variable ^^ {
      case realm ~ v =>
        val intrBase = PropertyReference(realm, FieldProperty("Intrinsics"))
        PropertyReference(intrBase, IndexProperty(getRefExpr(v)))
    } |
    // OrdinaryGetOwnProperty
    ("the value of" ~> variable <~ "'s") ~ ("[[" ~> word <~ "]]" ~ "attribute") ^^ {
      case v ~ a => PropertyReference(v, FieldProperty(a))
    } |
    // Set.prototype.add
    ("the List that is" ~> propRef) |
    // AsyncGeneratorCompleteStep
    ("the" ~> ordinal <~ "element") ~ ("of" ~> variable) ^^ {
      case o ~ x =>
        PropertyReference(x, IndexProperty(DecimalMathValueLiteral(o - 1)))
    } |
    // SetFunctionName, SymbolDescriptiveString
    (variable <~ "'s") ~ ("[[" ~> word <~ "]]") <~ "value" ^^ {
      case b ~ f => PropertyReference(b, FieldProperty(f))
    }

  // ---------------------------------------------------------------------------
  // metalanguage properties
  // ---------------------------------------------------------------------------
  given prop: PL[Property] = {
    ("." ~> "[[" ~> word <~ "]]") ^^ { FieldProperty(_) } |||
    ("." ~ "[[" ~> intr <~ "]]") ^^ { i => IntrinsicProperty(i) } |||
    (("'s" | ".") ~> camel) ^^ { ComponentProperty(_) } |||
    ("the" ~> component <~ opt("component") ~ "of") ^^ {
      ComponentProperty(_)
    } |||
    ("the" ~> nt <~ "of") ^^ { NonterminalProperty(_) } |||
    ("[" ~> expr <~ "]") ^^ { IndexProperty(_) }
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
    "%" ~> (word ~ rep("." ~> word)) <~ "%" ^^ {
      case b ~ ps => Intrinsic(b, ps)
    }
  }.named("lang.Intrinsic")

  // ---------------------------------------------------------------------------
  // metalanguage types
  // ---------------------------------------------------------------------------
  given ty: PL[Type] = {
    rep1(camel) ^^ { case ss => Type(ss.mkString(" ")) } |||
    "[a-zA-Z ]+ object".r ^^ { Type(_) } |||
    "\\w+ Environment Record".r ^^ { Type(_) } |||
    opt("ECMAScript code") ~ "execution context" ^^! {
      Type("ExecutionContext")
    } |||
    "List of" ~ word ^^! { Type("List") } |||
    nt ^^ { Type(_) }
  }.named("lang.Type")

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  // extensions for Parser for replacing `^^!`
  extension [T](p: Parser[T]) {
    def ^^![S](v: => S): Parser[S] = Parser { in => p(in).map(x => v) }
  }

  // html tags
  private lazy val tagStart: Parser[String] = "<[^>]+>".r
  private lazy val tagEnd: Parser[String] = "</[a-z-]+>".r

  // nonterminals
  private lazy val nt: Parser[String] = "|" ~> word <~ "|"

  // ordinal
  private lazy val ordinal: Parser[Int] =
    word.map(_.toIntFromOrdinal).filter(_.isDefined).map(_.get)

  // separators
  private def sep(s: Parser[Any]): Parser[Any] = (
    "," ||| "," ~ s ||| s
  )

  // verbs
  private def either[T](
    b: Parser[Boolean],
    p: Parser[T],
  ): Parser[Boolean ~ List[T]] =
    lazy val compoundGuard = guard(not("is" | ">"))
    ((b ^^ { case b => !b }) <~ "neither") ~ repsep(p, sep("nor")) |
    (b <~ "either") ~ p ~ ("or" ~> p) ^^ {
      case b ~ p0 ~ p1 => new ~(b, List(p0, p1))
    } |
    (b <~ opt("either")) ~ repsep(p <~ compoundGuard, sep("or")) |
    b ~ p ^^ { case b ~ p => new ~(b, List(p)) }
  private def isEither[T](p: Parser[T]): Parser[Boolean ~ List[T]] =
    either(isNeg, p)
  private def hasEither[T](p: Parser[T]): Parser[Boolean ~ List[T]] =
    either(hasNeg, p)
  private def isNeg: Parser[Boolean] =
    "is not" ^^! { true } | "is" ^^^ { false }
  private def areNeg: Parser[Boolean] =
    ("are both not" | "are not") ^^^ { true } | ("are both" | "are") ^^^ {
      false
    }
  private def hasNeg: Parser[Boolean] =
    "does not have" ^^^ { true } | "has" ^^^ { false }

  // helper for creating expressions, conditions
  private def getRefExpr(r: Reference): Expression = ReferenceExpression(r)
  private def getExprCond(e: Expression): Condition = ExpressionCondition(e)
}
