package esmeta.lang

import esmeta.lang.Utils.*
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
      suspendStep |
      ifStep |
      forEachStep |
      forEachIntStep |
      blockStep

  // let steps
  lazy val letStep: P[LetStep] =
    ("let" ~> variable <~ "be") ~ endWithExpr ^^ { case x ~ e => LetStep(x, e) }

  // set steps
  lazy val setStep: P[SetStep] =
    ("set" ~> ref <~ "to") ~ endWithExpr ^^ { case r ~ e => SetStep(r, e) }

  // if-then-else steps
  lazy val ifStep: P[IfStep] =
    ("if" ~> cond <~ "," ~ opt("then")) ~ step ~ opt(
      opt(subStepPrefix) ~ ("else" | "otherwise") ~ opt(",") ~> step,
    ) ^^ { case c ~ t ~ e => IfStep(c, t, e) }

  // return steps
  lazy val returnStep: P[ReturnStep] =
    "return" ~> end ^^^ { ReturnStep(None) } |
      "return" ~> endWithExpr ^^ { case e => ReturnStep(Some(e)) }

  // assertion steps
  lazy val assertStep: P[AssertStep] =
    "assert" ~ ":" ~> (upper ~> cond) <~ end ^^ { AssertStep(_) }

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
    opt("perform" | "call") ~> (invokeExpr | returnIfAbruptExpr) <~ end ^^ {
      PerformStep(_)
    }

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
    "push" ~> ref <~ (
      "onto the execution context stack;" ~ ref ~
        "is now the running execution context" ~ end
    ) ^^ { case r => PushStep(r) }

  // note steps
  lazy val noteStep: P[NoteStep] =
    ("NOTE" ~ ":") ~> ".*".r ^^ { str => NoteStep(str) }

  // suspend steps
  lazy val suspendStep: P[SuspendStep] =
    "suspend" ~> baseRef <~
      (opt("and remove it from the execution context stack") ~ end) ^^ {
        SuspendStep(_)
      }

  // block steps
  lazy val blockStep: P[BlockStep] = block ^^ { BlockStep(_) }

  // not yet supported steps
  lazy val yetStep: P[YetStep] = yetExpr ^^ { YetStep(_) }

  // end of step
  lazy val end: Parser[String] = "." <~ upper | ";"

  // end with expression
  lazy val endWithExpr: P[Expression] = expr <~ end | multilineExpr

  // ---------------------------------------------------------------------------
  // algorithm expressions
  // ---------------------------------------------------------------------------
  given expr: P[Expression] =
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
  lazy val multilineExpr: P[MultilineExpression] = closureExpr

  // string concatenation expressions
  lazy val stringConcatExpr: P[StringConcatExpression] =
    "the string-concatenation of" ~> repsep(expr, sep("and")) ^^ {
      StringConcatExpression(_)
    }

  // list concatenation expressions
  lazy val listConcatExpr: P[ListConcatExpression] =
    "the list-concatenation of" ~> repsep(expr, sep("and")) ^^ {
      ListConcatExpression(_)
    }

  // record expressions
  lazy val recordExpr: P[RecordExpression] =
    opt("the") ~> ty ~ ("{" ~> repsep((field <~ ":") ~ expr, ",") <~ "}") ^^ {
      case t ~ fs =>
        val fields = fs.map { case f ~ e => f -> e }
        RecordExpression(t, fields)
    }

  // `length of` expressions
  lazy val lengthExpr: P[LengthExpression] =
    "the length of" ~> expr ^^ { LengthExpression(_) } |||
      "the number of code" ~ ("units" | "unit elements") ~ "in" ~> expr ^^ {
        LengthExpression(_)
      }

  // `substring of` expressions
  lazy val substrExpr: P[SubstringExpression] =
    ("the substring of" ~> expr) ~
      ("from" ~> expr) ~
      ("to" ~> expr) ^^ { case e ~ f ~ t => SubstringExpression(e, f, t) }

  // `the number of elements in` expressions
  lazy val numberOfExpr: P[NumberOfExpression] =
    ("the number of elements in" ~ opt("the List") ~> expr) ^^ {
      NumberOfExpression(_)
    }

  // `source text` expressions
  lazy val sourceTextExpr: P[SourceTextExpression] =
    ("the source text matched by" ~> expr) ^^ { case e =>
      SourceTextExpression(e)
    }

  // abstract closure expressions
  lazy val closureExpr: P[AbstractClosureExpression] =
    lazy val params: P[List[Variable]] =
      "no parameters" ^^^ { Nil } |||
        "parameters" ~> ("(" ~> repsep(variable, ",") <~ ")")
    lazy val captured: P[List[Variable]] =
      "catpures nothing" ^^^ { Nil } |||
        "captures" ~> repsep(variable, sep("and"))

    "a new Abstract Closure with" ~> params ~ ("that" ~> captured) ~
      ("and performs the following steps when called:" ~> blockStep) ^^ {
        case ps ~ cs ~ body => AbstractClosureExpression(ps, cs, body)
      }

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
      "NewTarget" ^^^ NewTargetLiteral |||
      hexLiteral |||
      "`[^`]+`".r ^^ { case s => CodeLiteral(s.substring(1, s.length - 1)) } |||
      ntLiteral |||
      "~" ~> "[-+a-zA-Z0-9]+".r <~ "~" ^^ { ConstLiteral(_) } |||
      "the empty String" ^^^ StringLiteral("") |||
      strLiteral |||
      field ^^ { FieldLiteral(_) } |||
      "@@" ~> word ^^ { SymbolLiteral(_) } |||
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
      "*null*" ^^^ NullLiteral |||
      "absent" ^^^ AbsentLiteral |||
      "Undefined" ^^^ UndefinedTypeLiteral |||
      "Null" ^^^ NullTypeLiteral |||
      "Boolean" ^^^ BooleanTypeLiteral |||
      "String" ^^^ StringTypeLiteral |||
      "Symbol" ^^^ SymbolTypeLiteral |||
      "Number" ^^^ NumberTypeLiteral |||
      "BigInt" ^^^ BigIntTypeLiteral |||
      "Object" ^^^ ObjectTypeLiteral

  // code unit literals with hexadecimal numbers
  lazy val hexLiteral: P[HexLiteral] =
    (opt("the code unit") ~ "0x" ~> "[0-9A-F]+".r) ~
      opt("(" ~> "[ A-Z]+".r <~ ")") ^^ { case n ~ x =>
        HexLiteral(Integer.parseInt(n, 16), x)
      }

  // nonterminal literals
  lazy val ntLiteral: P[NonterminalLiteral] =
    opt("the") ~> opt(
      word.map(_.toIntFromOrdinal).filter(_.isDefined),
    ) ~ ("|" ~> word <~ "|") ^^ { case ord ~ x =>
      NonterminalLiteral(ord.flatten, x)
    }

  // string literals
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
      invokeNumericExpr |||
      invokeClosureExpr |||
      invokeAMExpr |||
      invokeSDOExpr

  // arguments for invocation epxressions
  lazy val invokeArgs: P[List[Expression]] = ("(" ~> repsep(expr, ",") <~ ")")

  // abstract operation (AO) invocation expressions
  lazy val invokeAOExpr: P[InvokeAbstractOperationExpression] =
    "(this)?[A-Z][a-zA-Z0-9/]*".r ~ invokeArgs ^^ { case x ~ as =>
      InvokeAbstractOperationExpression(x, as)
    }

  // numeric method invocation expression
  lazy val invokeNumericExpr: P[InvokeNumericMethodExpression] =
    ty ~ ("::" ~> "[A-Za-z]+".r) ~ invokeArgs ^^ { case t ~ op ~ as =>
      InvokeNumericMethodExpression(t, op, as)
    }

  // abstract closure invocation expression
  lazy val invokeClosureExpr: P[InvokeAbstractClosureExpression] =
    variable ~ invokeArgs ^^ { case v ~ as =>
      InvokeAbstractClosureExpression(v, as)
    }

  // method invocation expressions
  lazy val invokeAMExpr: P[InvokeMethodExpression] =
    (opt("<[^>]+>".r) ~> propRef <~ opt("</emu-meta>")) ~ invokeArgs ^^ {
      case p ~ as => InvokeMethodExpression(p, as)
    }

  // syntax-directed operation (SDO) invocation expressions
  lazy val invokeSDOExpr: P[InvokeSyntaxDirectedOperationExpression] =
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
      name ~ base ~ opt(argsPart) ^^ { case x ~ b ~ as =>
        InvokeSyntaxDirectedOperationExpression(b, x, as.getOrElse(Nil))
      }

    // Evalution SDO
    lazy val evalSDOExpr =
      "the result of evaluating" ~> expr ^^ { case b =>
        InvokeSyntaxDirectedOperationExpression(b, "Evaluation", Nil)
      }

    // Contains SDO
    lazy val containsSDOExpr =
      expr ~ ("Contains" ~> expr) ^^ { case b ~ arg =>
        InvokeSyntaxDirectedOperationExpression(b, "Contains", List(arg))
      }

    normalSDOExpr ||| evalSDOExpr ||| containsSDOExpr

  // return-if-abrupt expressions
  lazy val returnIfAbruptExpr: P[ReturnIfAbruptExpression] =
    ("?" ^^^ true | "!" ^^^ false) ~ invokeExpr ^^ { case c ~ e =>
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
    } ||| ("If" ~> baseCond) ~ (", then" ~> baseCond) ^^ { case l ~ r =>
      CompoundCondition(l, Imply, r)
    }

  // base conditions
  lazy val baseCond: P[Condition] =
    exprCond |||
      instanceOfCond |||
      hasFieldCond |||
      abruptCond |||
      isAreCond |||
      binCond

  // expression conditions
  lazy val exprCond: P[ExpressionCondition] =
    expr ^^ { ExpressionCondition(_) }

  // instance check conditions
  lazy val instanceOfCond: P[InstanceOfCondition] =
    expr ~ isNeg ~ (("a" | "an") ~> ty) ^^ { case e ~ n ~ t =>
      InstanceOfCondition(e, n, t)
    }

  // field includsion conditions
  lazy val hasFieldCond: P[HasFieldCondition] =
    lazy val fieldStr = "field" | ("internal" ~ ("method" | "slot"))
    expr ~
      ("has" ^^^ false ||| "does not have" ^^^ true) ~
      (("an" | "a") ~> field <~ fieldStr) ^^ { case e ~ n ~ f =>
        HasFieldCondition(e, n, f)
      }

  // abrupt completion check conditions
  lazy val abruptCond: P[AbruptCompletionCondition] =
    variable ~ isNeg <~ "an abrupt completion" ^^ { case x ~ n =>
      AbruptCompletionCondition(x, n)
    }

  // `A is/are B` condition
  lazy val isAreCond: P[IsAreCondition] =
    lazy val left: P[List[Expression]] =
      (opt("both") ~> expr) ~ ("and" ~> expr) <~ guard("are") ^^ {
        case e0 ~ e1 => List(e0, e1)
      } ||| expr <~ guard("is") ^^ { List(_) }

    lazy val are = "are" ~ opt("both")
    lazy val neg: P[Boolean] =
      isNeg | are ~ "not" ^^^ { true } | are ^^^ { false }

    lazy val right: P[(Boolean, List[Expression])] =
      (neg ~ expr ^^ { case n ~ e => (n, List(e)) } |||
        (neg <~ "either") ~ repsep(expr, sep("or")) ^^ { case n ~ es =>
          (n, es)
        } |||
        (neg <~ "neither") ~ repsep(expr, sep("nor")) ^^ { case n ~ es =>
          (!n, es)
        } |||
        neg <~ "present" ^^ { case n => (!n, List(AbsentLiteral)) })

    left ~ right ^^ { case l ~ (n, r) => IsAreCondition(l, n, r) }

  // binary conditions
  lazy val binCond: P[BinaryCondition] =
    import BinaryCondition.Op.*
    lazy val op: Parser[BinaryCondition.Op] =
      "=" ^^^ Eq |||
        "≠" ^^^ NEq |||
        "<" ^^^ LessThan |||
        "≤" ^^^ LessThanEqual |||
        ">" ^^^ GreaterThan |||
        "≥" ^^^ GreaterThanEqual |||
        "is the same sequence of code units as" ^^^ SameCodeUnits |||
        "contains" ^^^ Contains |||
        "does not contain" ^^^ NContains
    expr ~ op ~ expr ^^ { case l ~ o ~ r => BinaryCondition(l, o, r) }

  // ---------------------------------------------------------------------------
  // algorithm references
  // ---------------------------------------------------------------------------
  given ref: P[Reference] = baseRef ||| propRef

  // property references
  lazy val propRef: P[PropertyReference] =
    baseRef ~ rep1(prop) ^^ { case base ~ ps =>
      val (p :: rest) = ps
      rest.foldLeft[PropertyReference](PropertyReference(base, p))(
        PropertyReference(_, _),
      )
    } ||| ("the" ~> camel <~ opt("component")) ~ ("of" ~> variable) ^^ {
      case c ~ v => PropertyReference(v, ComponentProperty(c))
    }

  // base references
  lazy val baseRef: P[Reference] = variable |||
    "the" ~ opt("currently") ~ "running execution context" ^^^ {
      RunningExecutionContext
    } |||
    "the current Realm Record" ^^^ { CurrentRealmRecord } |||
    "the active function object" ^^^ { ActiveFunctionObject }

  // variables
  lazy val variable: P[Variable] =
    "_[^_]+_".r ^^ { case s => Variable(s.substring(1, s.length - 1)) }

  // ---------------------------------------------------------------------------
  // algorithm properties
  // ---------------------------------------------------------------------------
  given prop: P[Property] =
    ("." ~> field) ^^ { FieldProperty(_) } |||
      (("'s" | ".") ~> camel) ^^ { ComponentProperty(_) } |||
      ("[" ~> expr <~ "]") ^^ { IndexProperty(_) }

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

  // verbs
  private val isNeg: Parser[Boolean] =
    "is not" ^^^ true | "is" ^^^ false
  private val hasNeg: Parser[Boolean] =
    "does not have" ^^^ true | "has" ^^^ false
}
