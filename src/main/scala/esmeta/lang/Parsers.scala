package esmeta.lang

import esmeta.lang.Utils.*
import esmeta.util.{IndentParsers, Locational}
import scala.util.matching.Regex

/** language parsers */
trait Parsers extends IndentParsers {
  type P[T] = PackratParser[T]
  type PL[T <: Locational] = LocationalParser[T]

  // ---------------------------------------------------------------------------
  // algorithm blocks
  // ---------------------------------------------------------------------------
  given block: PL[Block] = indent ~> (
    rep1(subStep) ^^ { StepBlock(_) } |
    rep1(next ~ "*" ~> (expr <~ guard(EOL) | yetExpr)) ^^ { ExprBlock(_) } |
    next ~> figureStr ^^ { Figure(_) }
  ) <~ dedent

  // sub-steps
  lazy val subStepPrefix: Parser[Option[String]] =
    next ~> "1." ~> opt("[id=\"" ~> "[-a-zA-Z0-9]+".r <~ "\"]") <~ upper
  lazy val subStep: Parser[SubStep] =
    subStepPrefix ~ (step <~ guard(EOL) | yetStep) ^^ {
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
  given step: PL[Step] =
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
    suspendStep |
    ifStep |
    forEachStep |
    forEachIntStep |
    blockStep

  // let steps
  lazy val letStep: PL[LetStep] =
    ("let" ~> variable <~ "be") ~ endWithExpr ^^ { case x ~ e => LetStep(x, e) }

  // set steps
  lazy val setStep: PL[SetStep] =
    ("set" ~> ref <~ "to") ~ endWithExpr ^^ { case r ~ e => SetStep(r, e) }

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
    ("for each" ~ opt("element") ~> opt(ty)) ~
    variable ~
    ("of" ~> expr) ~
    ("," ~ opt("do") ~> step) ^^ {
      case t ~ r ~ e ~ s =>
        ForEachStep(t, r, e, s)
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

  // throw steps
  lazy val throwStep: PL[ThrowStep] =
    "throw a *" ~> word <~ "* exception" <~ end ^^ { ThrowStep(_) }

  // perform steps
  lazy val performStep: PL[PerformStep] =
    opt("perform" | "call") ~> (invokeExpr | returnIfAbruptExpr) <~ end ^^ {
      PerformStep(_)
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

  // push steps
  lazy val pushStep: PL[PushStep] =
    "push" ~> ref <~ (
      "onto the execution context stack;" ~ ref ~
      "is now the running execution context" ~ end
    ) ^^ { case r => PushStep(r) }

  // note steps
  lazy val noteStep: PL[NoteStep] =
    ("NOTE" ~ ":") ~> ".*".r ^^ { str => NoteStep(str) }

  // suspend steps
  lazy val suspendStep: PL[SuspendStep] =
    "suspend" ~> baseRef <~
    (opt("and remove it from the execution context stack") ~ end) ^^ {
      SuspendStep(_)
    }

  // block steps
  lazy val blockStep: PL[BlockStep] = block ^^ { BlockStep(_) }

  // not yet supported steps
  lazy val yetStep: PL[YetStep] = yetExpr ^^ { YetStep(_) }

  // end of step
  lazy val end: Parser[String] = "." <~ upper | ";"

  // end with expression
  lazy val endWithExpr: PL[Expression] = expr <~ end | multilineExpr

  // ---------------------------------------------------------------------------
  // algorithm expressions
  // ---------------------------------------------------------------------------
  given expr: PL[Expression] =
    stringConcatExpr |||
    listConcatExpr |||
    recordExpr |||
    lengthExpr |||
    substrExpr |||
    numberOfExpr |||
    sourceTextExpr |||
    intrExpr |||
    calcExpr |||
    invokeExpr |||
    returnIfAbruptExpr |||
    listExpr

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
    opt("the") ~> ty ~ ("{" ~> repsep((field <~ ":") ~ expr, ",") <~ "}") ^^ {
      case t ~ fs =>
        val fields = fs.map { case f ~ e => f -> e }
        RecordExpression(t, fields)
    }

  // `length of` expressions
  lazy val lengthExpr: PL[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) } |||
    "the number of code" ~ ("units" | "unit elements") ~ "in" ~> expr ^^ {
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

  // abstract closure expressions
  lazy val closureExpr: PL[AbstractClosureExpression] =
    lazy val params: P[List[Variable]] =
      "no parameters" ^^! { Nil } |||
      "parameters" ~> ("(" ~> repsep(variable, ",") <~ ")")
    lazy val captured: P[List[Variable]] =
      "catpures nothing" ^^! { Nil } |||
      "captures" ~> repsep(variable, sep("and"))

    "a new Abstract Closure with" ~> params ~ ("that" ~> captured) ~
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
      refExpr ||| literal ||| mathOpExpr ||| "(" ~> calc <~ ")" ||| (
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

  // reference expressions
  lazy val refExpr: PL[ReferenceExpression] =
    ref ^^ { ReferenceExpression(_) }

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
  lazy val literal: PL[Literal] =
    opt("the") ~> "*this* value" ^^! ThisLiteral() |||
    "NewTarget" ^^! NewTargetLiteral() |||
    hexLiteral |||
    "`[^`]+`".r ^^ { case s => CodeLiteral(s.substring(1, s.length - 1)) } |||
    ntLiteral |||
    "~" ~> "[-+a-zA-Z0-9]+".r <~ "~" ^^ { ConstLiteral(_) } |||
    "the empty String" ^^! StringLiteral("") |||
    strLiteral |||
    field ^^ { FieldLiteral(_) } |||
    "@@" ~> word ^^ { SymbolLiteral(_) } |||
    "+∞" ^^! PositiveInfinityMathValueLiteral() |||
    "-∞" ^^! NegativeInfinityMathValueLiteral() |||
    number ^^ { case s => DecimalMathValueLiteral(BigDecimal(s)) } |||
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

  // code unit literals with hexadecimal numbers
  lazy val hexLiteral: PL[HexLiteral] =
    (opt("the code unit") ~ "0x" ~> "[0-9A-F]+".r) ~
    opt("(" ~> "[ A-Z]+".r <~ ")") ^^ {
      case n ~ x =>
        HexLiteral(Integer.parseInt(n, 16), x)
    }

  // nonterminal literals
  lazy val ntLiteral: PL[NonterminalLiteral] =
    opt("the") ~> opt(
      word.map(_.toIntFromOrdinal).filter(_.isDefined),
    ) ~ ("|" ~> word <~ "|") ^^ {
      case ord ~ x =>
        NonterminalLiteral(ord.flatten, x)
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

  // algorithm invocation expressions
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
    "(this)?[A-Z][a-zA-Z0-9/]*".r ~ invokeArgs ^^ {
      case x ~ as =>
        InvokeAbstractOperationExpression(x, as)
    }

  // numeric method invocation expression
  lazy val invokeNumericExpr: PL[InvokeNumericMethodExpression] =
    ty ~ ("::" ~> "[A-Za-z]+".r) ~ invokeArgs ^^ {
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
    (opt("<[^>]+>".r) ~> propRef <~ opt("</emu-meta>")) ~ invokeArgs ^^ {
      case p ~ as => InvokeMethodExpression(p, as)
    }

  // syntax-directed operation (SDO) invocation expressions
  lazy val invokeSDOExpr: PL[InvokeSyntaxDirectedOperationExpression] =
    lazy val name = (opt("the result of performing" | "the") ~> word)
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
      "the result of evaluating" ~> expr ^^ {
        case b =>
          InvokeSyntaxDirectedOperationExpression(b, "Evaluation", Nil)
      }

    // Contains SDO
    lazy val containsSDOExpr =
      expr ~ ("Contains" ~> expr) ^^ {
        case b ~ arg =>
          InvokeSyntaxDirectedOperationExpression(b, "Contains", List(arg))
      }

    normalSDOExpr ||| evalSDOExpr ||| containsSDOExpr

  // return-if-abrupt expressions
  lazy val returnIfAbruptExpr: PL[ReturnIfAbruptExpression] =
    ("?" ^^! true | "!" ^^! false) ~ invokeExpr ^^ {
      case c ~ e =>
        ReturnIfAbruptExpression(e, c)
    }

  // list expressions
  lazy val listExpr: PL[ListExpression] =
    "a new empty List" ^^! ListExpression(Nil) |||
    "«" ~> repsep(expr, ",") <~ "»" ^^ { ListExpression(_) } |||
    "a List whose sole element is" ~> expr ^^ { e => ListExpression(List(e)) }

  // not yet supported expressions
  lazy val yetExpr: PL[YetExpression] =
    opt("[YET]") ~> ".+".r ~ opt(block) ^^ { case s ~ b => YetExpression(s, b) }

  // ---------------------------------------------------------------------------
  // algorithm conditions
  // ---------------------------------------------------------------------------
  given cond: PL[Condition] =
    import CompoundCondition.Op.*
    lazy val op: P[CompoundCondition.Op] = "and" ^^! And ||| "or" ^^! Or
    baseCond ~ rep(op ~ baseCond) ^^ {
      case l ~ rs =>
        rs.foldLeft(l) { case (l, op ~ r) => CompoundCondition(l, op, r) }
    } ||| ("If" ~> baseCond) ~ (", then" ~> baseCond) ^^ {
      case l ~ r => CompoundCondition(l, Imply, r)
    }

  // base conditions
  lazy val baseCond: PL[Condition] =
    exprCond |||
    instanceOfCond |||
    hasFieldCond |||
    abruptCond |||
    isAreCond |||
    binCond

  // expression conditions
  lazy val exprCond: PL[ExpressionCondition] =
    expr ^^ { ExpressionCondition(_) }

  // instance check conditions
  lazy val instanceOfCond: PL[InstanceOfCondition] =
    expr ~ isEither((("an" | "a") ~> ty)) ^^ {
      case e ~ (n ~ t) =>
        InstanceOfCondition(e, n, t)
    }

  // field includsion conditions
  lazy val hasFieldCond: PL[HasFieldCondition] =
    lazy val fieldStr = "field" | ("internal" ~ ("method" | "slot"))
    expr ~
    ("has" ^^! false ||| "does not have" ^^! true) ~
    (("an" | "a") ~> field <~ fieldStr) ^^ {
      case e ~ n ~ f =>
        HasFieldCondition(e, n, f)
    }

  // abrupt completion check conditions
  lazy val abruptCond: PL[AbruptCompletionCondition] =
    variable ~ isNeg <~ "an abrupt completion" ^^ {
      case x ~ n =>
        AbruptCompletionCondition(x, n)
    }

  // `A is/are B` condition
  lazy val isAreCond: PL[IsAreCondition] =
    lazy val left: P[List[Expression]] =
      (opt("both") ~> expr) ~ ("and" ~> expr) <~ guard("are") ^^ {
        case e0 ~ e1 => List(e0, e1)
      } ||| expr <~ guard("is") ^^ { List(_) }

    lazy val are = "are" ~ opt("both")
    lazy val neg: P[Boolean] =
      isNeg | are ~ "not" ^^! { true } | are ^^! { false }

    lazy val right: P[Boolean ~ List[Expression]] =
      either(neg, expr) |||
      neg <~ "present" ^^ { case n => new ~(!n, List(AbsentLiteral())) }

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
    expr ~ (isNeg <~ "an element of") ~ expr ^^ {
      case l ~ n ~ r =>
        BinaryCondition(r, if (n) NContains else Contains, l)
    }

  // ---------------------------------------------------------------------------
  // algorithm references
  // ---------------------------------------------------------------------------
  given ref: PL[Reference] = baseRef ||| propRef

  // property references
  lazy val propRef: PL[PropertyReference] = baseRef ~ rep1(prop) ^^ {
    case base ~ ps =>
      val (p :: rest) = ps
      rest.foldLeft[PropertyReference](PropertyReference(base, p))(
        PropertyReference(_, _),
      )
  } ||| ("the" ~> camel <~ opt("component")) ~ ("of" ~> variable) ^^ {
    case c ~ v => PropertyReference(v, ComponentProperty(c))
  }

  // base references
  lazy val baseRef: PL[Reference] =
    variable |||
    "the" ~ opt("currently") ~ "running execution context" ^^! {
      RunningExecutionContext()
    } |||
    "the current Realm Record" ^^! { CurrentRealmRecord() } |||
    "the active function object" ^^! { ActiveFunctionObject() }

  // variables
  lazy val variable: PL[Variable] = "_[^_]+_".r ^^ {
    case s => Variable(s.substring(1, s.length - 1))
  }

  // ---------------------------------------------------------------------------
  // algorithm properties
  // ---------------------------------------------------------------------------
  given prop: PL[Property] =
    ("." ~> field) ^^ { FieldProperty(_) } |||
    (("'s" | ".") ~> camel) ^^ { ComponentProperty(_) } |||
    ("[" ~> expr <~ "]") ^^ { IndexProperty(_) }

  // ---------------------------------------------------------------------------
  // algorithm fields
  // ---------------------------------------------------------------------------
  given field: PL[Field] =
    "[[" ~> (word ^^ { StringField(_) } | intr ^^ { IntrinsicField(_) }) <~ "]]"

  // ---------------------------------------------------------------------------
  // algorithm intrinsics
  // ---------------------------------------------------------------------------
  given intr: PL[Intrinsic] = "%" ~> (word ~ rep("." ~> word)) <~ "%" ^^ {
    case b ~ ps => Intrinsic(b, ps)
  }

  // ---------------------------------------------------------------------------
  // algorithm types
  // ---------------------------------------------------------------------------
  given ty: PL[Type] =
    rep1(camel) ^^ { case ss => Type(ss.mkString(" ")) } |||
    ("|" ~> word <~ "|") ^^ { case nt => Type(s"|$nt|") }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  // extensions for Parser for replacing `^^!`
  extension [T](p: Parser[T]) {
    def ^^![S](v: => S): Parser[S] = Parser { in => p(in).map(x => v) }
  }

  // implicit conversion from parsers to packrat parsers
  private implicit def parser2loc[T <: Locational](
    p: => Parser[T],
  ): PL[T] = {
    lazy val q = p
    lazy val packrat = parser2packrat(q)
    locationed(packrat)
  }

  // separators
  private def sep(s: Parser[Any]): Parser[Any] = (
    "," ||| "," ~ s ||| s
  )

  // verbs
  private def either[T](
    b: Parser[Boolean],
    p: Parser[T],
  ): Parser[Boolean ~ List[T]] =
    ((b ^^ { case b => !b }) <~ "neither") ~ repsep(p, sep("nor")) |
    (b <~ "either") ~ repsep(p, sep("or")) |
    b ~ p ^^ { case b ~ p => new ~(b, List(p)) }
  private def isEither[T](p: Parser[T]): Parser[Boolean ~ List[T]] =
    either(isNeg, p)
  private def hasEither[T](p: Parser[T]): Parser[Boolean ~ List[T]] =
    either(hasNeg, p)
  private def isNeg: Parser[Boolean] =
    "is not" ^^! { true } | "is" ^^! { false }
  private def hasNeg: Parser[Boolean] =
    "does not have" ^^! { true } | "has" ^^! { false }
}
