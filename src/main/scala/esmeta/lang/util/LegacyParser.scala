package esmeta.lang.util

import esmeta.lang.*

/** metalanguage parser extended with phrasings of older editions (ES2016-)
  *
  * Each legacy phrasing is desugared into the construct that later editions
  * use for the same operation (e.g., `Increase _k_ by 1.` into `Set _k_ to _k_
  * + 1.`, a List of code units into a String). It only adds alternatives
  * through the extension points and the if/repeat steps, so the current
  * edition parses as with `Parser` except for steps that were not yet
  * supported.
  */
object LegacyParser extends LegacyParsers
trait LegacyParsers extends Parsers {
  import BinaryExpressionOperator.*

  override def extraStep: PL[Step] = legacyStep
  override def extraExpr: PL[Expression] = legacyExpr
  override def extraCond: PL[Condition] = legacyCond

  // ---------------------------------------------------------------------------
  // steps
  // ---------------------------------------------------------------------------
  lazy val legacyStep: PL[Step] =
    incStep |
    concatBlockStep |
    commentedLetStep |
    commaIfStep |
    removeFirstLetStep |
    appendCodeUnitsStep |
    appendLastStep |
    searchStep |
    returnSearchStep |
    letSearchStep |
    forEachIntStartingStep |
    noActionStep

  // `Increase _k_ by 1.`
  lazy val incStep: PL[Step] =
    (("increase" | "increment") ^^^ Add | ("decrease" | "decrement") ^^^ Sub) ~
    ref ~ ("by" ~> calcExpr) <~ lend ^^ {
      case op ~ r ~ e => SetStep(r, BinaryExpression(refExpr(r), op, e))
    }

  // `Let _p1_ be the string-concatenation of:` followed by a list of items
  lazy val concatBlockStep: PL[Step] =
    ("let" ~> variable <~ "be") ~ concatBlockExpr ^^ {
      case x ~ e => LetStep(x, e)
    } |
    ("set" ~> ref <~ "to") ~ concatBlockExpr ^^ { case r ~ e => SetStep(r, e) }

  // `Let _x_ be <expr>. <explanatory sentence>`
  lazy val commentedLetStep: PL[Step] =
    lazy val comment =
      "\\. If _[^_]+_ is 0, the first element of the concatenation will be the empty String\\.".r |
      "\\. The definition of white space is .*".r
    ("let" ~> variable) ~ ("be" ~> expr) <~ comment ^^ {
      case x ~ e => LetStep(x, e)
    }

  // `If <cond>, let _x_ be <expr>, else <step>`
  lazy val commaIfStep: PL[Step] =
    ("if" ~> cond <~ ",") ~ ("let" ~> variable) ~ ("be" ~> expr <~ ",") ~
    ("else" ~> step) ^^ {
      case c ~ x ~ e ~ s => IfStep(c, LetStep(x, e), Some(s))
    }

  // `Remove the first element from _items_ and let _E_ be the value of that
  // element.`
  lazy val removeFirstLetStep: PL[Step] =
    ("remove the first element from" ~> ref) ~
    ("and let" ~> variable <~ "be the value of" ~ ("that" | "the") ~
    "element") <~ lend ^^ {
      case r ~ x =>
        block(
          LetStep(x, refExpr(PositionalElement(r, true))),
          RemoveStep(RemoveStep.Target.First(None), "from", refExpr(r)),
        )
    }

  // `Append in order the code unit elements of _s_ to the end of _list_.`
  // `Append the elements of the UTF16Encoding of _cp_ to the end of _list_.`
  // A List of code units is a String: the later editions use
  // string-concatenation for the same steps.
  lazy val appendCodeUnitsStep: PL[Step] =
    lazy val elems =
      opt("in order") ~ "the code unit elements of" ~> expr |
      "the elements of the UTF16Encoding of" ~> expr ^^ {
        e => InvokeAbstractOperationExpression("UTF16EncodeCodePoint", List(e))
      }
    ("append" ~> elems) ~ ("to the end of" ~> ref) <~ lend ^^ {
      case e ~ r => SetStep(r, StringConcatExpression(List(refExpr(r), e)))
    }

  // `Append _T_ as the last element of _list_.`
  lazy val appendLastStep: PL[Step] =
    ("append" ~> expr) ~ ("as the last element of" ~> ref) <~ lend ^^ {
      case e ~ r => AppendStep(e, r)
    }

  // `Search _string_ for the first occurrence of _searchString_ and let _pos_
  // be the index within _string_ of the first code unit of the matched
  // substring and let _matched_ be _searchString_. If no occurrences of
  // _searchString_ were found, return _string_.`
  lazy val searchStep: PL[Step] =
    ("search" ~> variable) ~ ("for the first occurrence of" ~> variable) ~
    ("and let" ~> variable <~ "be the index within" ~ variable ~
    "of the first code unit of the matched substring") ~
    ("and let" ~> variable) ~ ("be" ~> expr <~ ".") ~
    ("If no occurrences of" ~ variable ~ "were found, return" ~> expr) <~
    lend ^^ {
      case s ~ t ~ p ~ m ~ e ~ d =>
        block(
          LetStep(
            p,
            InvokeAbstractOperationExpression(
              "StringIndexOf",
              List(refExpr(s), refExpr(t), DecimalMathValueLiteral(0)),
            ),
          ),
          LetStep(m, e),
          IfStep(
            IsAreCondition(List(refExpr(p)), false, List(minusOne)),
            ReturnStep(d),
            None,
          ),
        )
    }

  // `Return the smallest possible integer _k_ not smaller than _start_ such
  // that <cond>; but if there is no such integer _k_, return the value `-1`.`
  lazy val returnSearchStep: PL[Step] =
    ("return the" ~> searchBound) ~ ("such that" ~> cond) ~
    (";" ~ noSuchInteger ~ "return" ~> expr) <~ lend ^^ {
      case (x, asc, from) ~ c ~ d =>
        val (lo, hi, hiIncl) =
          if (asc) (from, PositiveInfinityMathValueLiteral(), false)
          else (DecimalMathValueLiteral(0), from, true)
        block(
          ForEachIntegerStep(
            x,
            lo,
            true,
            hi,
            hiIncl,
            asc,
            IfStep(c, ReturnStep(refExpr(x)), None),
          ),
          ReturnStep(d),
        )
    }

  // `Let _k_ be the largest possible non-negative integer not larger than
  // _start_ such that <cond>; but if there is no such integer, let _k_ be -1.`
  // It is a loop that moves _k_ until <cond> holds.
  lazy val letSearchStep: PL[Step] =
    lazy val kind =
      ("smallest" ^^^ true | "largest" ^^^ false) <~
      opt("possible") ~ opt("non-negative" | "nonnegative") ~ "integer"
    lazy val bound =
      ("not smaller than" | "not larger than") ~> calcExpr
    ("let" ~> variable) ~ ("be the" ~> kind) ~ opt(bound) ~
    ("such that" ~> cond) ~
    opt(";" ~ noSuchInteger ~ "let" ~ variable ~ "be" ~> expr) <~ lend ^^ {
      case x ~ asc ~ b ~ c ~ _ =>
        val init = b.getOrElse(DecimalMathValueLiteral(0))
        val notFound = negate(c)
        val loopCond =
          if (asc) notFound
          else
            CompoundCondition(
              BinaryCondition(
                refExpr(x),
                BinaryConditionOperator.GreaterThanEqual,
                DecimalMathValueLiteral(0),
              ),
              CompoundConditionOperator.And,
              notFound,
            )
        block(
          LetStep(x, init),
          RepeatStep(
            RepeatStep.LoopCondition.While(loopCond),
            SetStep(
              x,
              BinaryExpression(refExpr(x), if (asc) Add else Sub, one),
            ),
          ),
        )
    }

  // `For each integer _i_ starting with _fromIndex_ such that _i_ ≤ _len_ -
  // _searchLen_, in ascending order, do`
  lazy val forEachIntStartingStep: PL[Step] =
    ("for each" ~ opt("non-negative") ~ "integer" ~> variable) ~
    ("starting with" ~> calcExpr) ~
    ("such that" ~ variable ~ "≤" ~> calcExpr) ~
    ("," ~ "in" ~> ("ascending" ^^^ true | "descending" ^^^ false) <~
    "order," ~ opt("do")) ~ step ^^ {
      case x ~ from ~ hi ~ asc ~ body =>
        if (asc) ForEachIntegerStep(x, from, true, hi, true, true, body)
        else
          // descending from _from_: the `such that` bound is not implied by
          // the range, so keep it as a guard on the body
          ForEachIntegerStep(
            x,
            DecimalMathValueLiteral(0),
            true,
            from,
            true,
            false,
            IfStep(
              BinaryCondition(
                refExpr(x),
                BinaryConditionOperator.LessThanEqual,
                hi,
              ),
              body,
              None,
            ),
          )
    }

  // `No action is required.`
  lazy val noActionStep: PL[Step] =
    "no action is required" ~ lend ^^! NoteStep("No action is required.")

  // `Repeat, while <cond>` followed by a block (without `, do`)
  override lazy val repeatStep: PL[RepeatStep] =
    import RepeatStep.LoopCondition.*
    ("repeat" ~ opt(",")) ~> (
      "while" ~> cond <~ opt(",") ^^ { While(_) } |
      "until" ~> cond <~ opt(",") ^^ { Until(_) } |
      "" ^^! NoCondition
    ) ~ step ^^ {
      case c ~ s => RepeatStep(c, s)
    }

  // if-then-else steps with legacy else parts:
  // - `Else <cond>,` followed by a block (the condition is a comment)
  // - `; but if there is no such integer _k_, <step>`
  override lazy val ifStep: PL[IfStep] =
    val ifPart = "if" ~> (
      cond <~ ", then" | yetCond(", then") |
      (cond <~ ",")
    ) ~ (step | yetStep)
    val legacyElse =
      exists(subStepPrefix) ~ ("else" | "otherwise") ~ "[^\n]*,".r ~>
      blockStep
    val noSuchElse = noSuchInteger ~> (step | yetStep)
    val elsePart =
      exists(subStepPrefix) ~
      ("else" | "otherwise") ~
      exists(",") ~
      (step | yetStep) ^^ { case n ~ k ~ c ~ e => (e, n, k, c) }
    ifPart ~ opt(
      legacyElse ^^ { e => (e, true, "else", true) } |
      noSuchElse ^^ { e => (e, false, "but if there is no such", true) } |
      elsePart,
    ) ^^ {
      case c ~ t ~ es =>
        import IfStep.ElseConfig
        val (e, config) = es match
          case Some((e, n, k, c)) => (Some(e), ElseConfig(n, k.toLowerCase, c))
          case None               => (None, ElseConfig())
        IfStep(c, t, e, config)
    }

  // ---------------------------------------------------------------------------
  // expressions
  // ---------------------------------------------------------------------------
  lazy val legacyExpr: PL[Expression] =
    concatExpr |
    concatBlockExpr |
    legacySubstrExpr |
    legacyTrimExpr |
    legacyCodeUnitExpr |
    codeUnitListExpr |
    argumentsExpr |
    strictEqExpr |
    magnitudeExpr |
    legacyLiteral

  // string concatenations
  lazy val concatExpr: PL[Expression] =
    lazy val pre =
      ("the" | "a new" | "a") ~ "String value" ~ (
        "produced by concatenating" |
        ("that is" | "computed by") ~ "the concatenation of"
      ) | "the String formed by concatenating"
    pre ~> rep1sep(expr, lsep("and")) ^^ { StringConcatExpression(_) } |
    ("the String value consisting of the code units of" ~> expr) ~
    ("followed by the code units of" ~> expr) ^^ {
      case l ~ r => StringConcatExpression(List(l, r))
    } |
    "the previous value of" ~> expr

  // string concatenations of a list of items
  lazy val concatBlockExpr: PL[Expression] =
    lazy val item: Parser[Expression] =
      "[Tt]he String value of".r ~> expr <~ guard(EOL) |
      "[Cc]ode unit".r ~> expr <~ guard(EOL) |
      expr <~ guard(EOL) |
      yetExpr
    (
      ("the" | "a") ~ "String value that is the concatenation of the" ~
      "following String values:" |
      "the string-concatenation of:"
    ) ~> (indent ~> rep1(next ~ "*" ~> item) <~ dedent) ^^ {
      StringConcatExpression(_)
    }

  // substrings
  lazy val legacySubstrExpr: PL[Expression] =
    lazy val str = ("a String value" | "a String" | "the String value")
    lazy val unit = ("code units" | "code unit" | "elements" | "element")
    // `the substring of _S_ consisting of the code units at indices _p_
    // (inclusive) through _q_ (exclusive)`
    (opt(str ~ "equal to") ~ "the substring of" ~> expr) ~
    ("consisting of the code units at indices" ~> calcExpr) ~
    ("(inclusive) through" ~> calcExpr <~ "(exclusive)") ^^ {
      case s ~ f ~ t => SubstringExpression(s, f, Some(t))
    } |
    // `a String containing _n_ consecutive code units from _S_ beginning
    // with the code unit at index _i_`
    (str ~ "containing" ~> calcExpr) ~
    ("consecutive" ~ unit ~ "from" ~> expr) ~
    ("beginning with the" ~ unit ~ "at index" ~> calcExpr) ^^ {
      case n ~ s ~ i =>
        SubstringExpression(s, i, Some(BinaryExpression(i, Add, n)))
    } |
    // `a String whose length is _to_ - _from_, containing code units from
    // _S_, namely the code units with indices _from_ through _to_ - 1, in
    // ascending order`
    (str ~ "whose length is" ~ calcExpr ~ "," ~ "containing code units from" ~>
    expr) ~ ("," ~ "namely the code units with indices" ~> calcExpr) ~
    ("through" ~> calcExpr <~ "," ~ "in ascending order") ^^ {
      case s ~ f ~ t => SubstringExpression(s, f, Some(succ(t)))
    } |
    // `a String of length 1, containing one code unit from _S_, namely the
    // code unit at index _i_`
    (str ~ "of length 1, containing one code unit from" ~> expr) ~
    ("," ~ "namely the code unit at index" ~> calcExpr) ^^ {
      case s ~ i => SubstringExpression(s, i, Some(succ(i)))
    } |
    // `the first _n_ code units of _S_`
    ("the first" ~> calcExpr) ~ ("code units of" ~> expr) ^^ {
      case n ~ s => SubstringExpression(s, DecimalMathValueLiteral(0), Some(n))
    } |
    // `the trailing substring of _S_ starting at index _i_`
    ("the trailing substring of" ~> expr) ~ ("starting at index" ~> calcExpr) ^^ {
      case s ~ i => SubstringExpression(s, i, None)
    }

  // `a String value that is a copy of _S_ with both leading and trailing
  // white space removed`
  lazy val legacyTrimExpr: PL[Expression] =
    ("a String value that is a copy of" ~> expr) <~
    "with both leading and trailing white space removed" ^^ {
      TrimExpression(_, true, true)
    }

  // code units
  lazy val legacyCodeUnitExpr: PL[Expression] =
    import ConversionExpressionOperator.*
    // `the code unit at index _i_ of _S_`
    ("the code unit at index" ~> calcExpr) ~ ("of" ~> expr) ^^ {
      case i ~ s => CodeUnitAtExpression(s, i)
    } |
    // `the code unit value of the element at index _i_ in the String _S_`
    ("the code unit value of the element at index" ~> calcExpr) ~
    ("in the String" ~> expr) ^^ {
      case i ~ s =>
        ConversionExpression(
          ToMath,
          CodeUnitAtExpression(s, i),
          ConversionExpressionForm.Text("the", "of"),
        )
    } |
    "a value of Number type, whose value is" ~> expr ^^ {
      ConversionExpression(ToNumber, _, ConversionExpressionForm.Text("a", "of"))
    } |
    "the integer that is" ~> expr |
    "the code point" ~> variable ^^ { refExpr(_) } |
    "the number of code units in" ~> expr ^^ { LengthExpression(_) } |
    "the number of elements of" ~> expr ^^ {
      NumberOfExpression("elements", None, _, None)
    }

  // `the String value whose code units are, in order, the elements in the
  // List _L_. If _L_ has no elements, the empty String is returned.`
  // A List of code units is a String (see `appendCodeUnitsStep`); the
  // `StringExpression` wrapper marks the List as one.
  lazy val codeUnitListExpr: PL[Expression] =
    ("the String value whose" ~ ("code units" | "elements") ~ "are" ~
    opt(", in order,") ~ "the elements in the List" ~> expr) <~
    opt("\\. If [^.]* is returned".r) ^^ { StringExpression(_) }

  // arguments of built-in functions
  lazy val argumentsExpr: PL[Expression] =
    (
      "a List whose elements are, in left to right order, the arguments" ~
      "that were passed to this function invocation" |
      "a List whose elements are the arguments passed to this function" |
      ("the List of" | "a List containing the") ~
      "arguments passed to this function"
    ) ^^! argsList |
    ("a List whose first element is" ~> expr) <~
    "and whose subsequent elements are, in left to right order," ~
    "the arguments that were passed to this function invocation" ^^ {
      e =>
        ListConcatExpression(
          List(ListExpression(ListExpressionForm.LiteralSyntax(List(e))), argsList),
        )
    } |
    (
      "the number of actual arguments" |
      "the actual number of arguments passed to this function"
    ) ^^! NumberOfExpression("elements", None, argsList, None)

  // `the result of performing Strict Equality Comparison _x_ === _y_`
  // (the edition's own algorithm, renamed IsStrictlyEqual in ES2022)
  lazy val strictEqExpr: PL[Expression] =
    ("the result of performing Strict Equality Comparison" ~> expr) ~
    ("===" ~> expr) ^^ {
      case l ~ r =>
        InvokeAbstractOperationExpression("Strict Equality Comparison", List(l, r))
    } |
    "the result of" ~ guard("?" | "!") ~> returnIfAbruptExpr

  // `the mathematical value that is the same sign as _number_ and whose
  // magnitude is floor(abs(_number_))` (truncate in later editions)
  lazy val magnitudeExpr: PL[Expression] =
    (("the" ~ ("mathematical" | "Number") ~ "value") ~
    ("that is the same sign as" | "whose sign is the sign of") ~> calcExpr) <~
    "and whose magnitude is" ~ calcExpr ^^ {
      e => MathFuncExpression(MathFuncExpressionOperator.Truncate, List(e))
    }

  // literals
  lazy val legacyLiteral: PL[Expression] =
    // `*+0*` (later `*+0*<sub>𝔽</sub>`)
    "\\*[-+]?(∞|[0-9]+(\\.[0-9]+)?)\\*(?!<sub>)".r ^^ { s =>
      s.drop(1).dropRight(1) match
        case "+∞" | "∞" => NumberLiteral(Double.PositiveInfinity)
        case "-∞"       => NumberLiteral(Double.NegativeInfinity)
        case n          => NumberLiteral(n.toDouble)
    } |
    "@@" ~> word ^^ { SymbolLiteral(_) } |
    "a String consisting solely of" ~> hexLiteral |
    "zero" ^^! DecimalMathValueLiteral(0) |
    ("the empty string" | "the empty String" ~ "`\"\"`") ^^! StringLiteral(
      "",
      StringLiteralForm.EmptyString,
    ) |
    "the single-element String" ~> expr

  // ---------------------------------------------------------------------------
  // conditions
  // ---------------------------------------------------------------------------
  lazy val legacyCond: PL[Condition] =
    import BinaryConditionOperator.*
    lazy val cmp: Parser[BinaryConditionOperator] =
      "is greater than" ^^^ GreaterThan |
      "is less than" ^^^ LessThan |
      ("is not greater than" | "is not larger than") ^^^ LessThanEqual |
      ("is not less than" | "is not smaller than") ^^^ GreaterThanEqual
    lazy val supplied: Parser[Boolean] =
      ("was supplied" | "was passed" | "is supplied") ^^^ false |
      ("was not supplied" | "was not passed" | "was not provided" |
      "is not provided") ^^^ true
    // `_x_ is greater than _y_`
    expr ~ cmp ~ expr ^^ { case l ~ o ~ r => BinaryCondition(l, o, r) } |
    // `_x_ was supplied`
    (opt("argument") ~> expr) ~ supplied ^^ {
      case e ~ n => PredicateCondition(e, n, PredicateConditionOperator.Present)
    } |
    "no arguments were passed to this function invocation" ^^! {
      PredicateCondition(argsList, false, PredicateConditionOperator.Empty)
    } |
    // `the sequence of elements of _S_ starting at _start_ of length _n_ is
    // the same as the full element sequence of _T_`
    ("the sequence of" ~ ("elements" | "code units") ~ "of" ~> expr) ~
    ("starting at" ~> calcExpr) ~ ("of length" ~> calcExpr) ~
    ("is the same as the full" ~ ("element" | "code unit") ~ "sequence of" ~>
    expr) ^^ {
      case s ~ f ~ n ~ t =>
        IsAreCondition(
          List(SubstringExpression(s, f, Some(BinaryExpression(f, Add, n)))),
          false,
          List(t),
        )
    } |
    // `there exists any integer _k_ not smaller than _start_ such that <cond>`
    ("there exists" ~ opt("any" | "an") ~ "integer" ~> variable) ~ (
      "not smaller than" ~> calcExpr ^^ {
        f => (f, true, PositiveInfinityMathValueLiteral(), false)
      } |
      ("between" ~> calcExpr <~ "(inclusive)") ~
      ("and" ~> calcExpr <~ "(exclusive)") ^^ {
        case f ~ t => (f, true, t, false)
      }
    ) ~ ("such that" ~> cond) ^^ {
      case x ~ ((f, fi, t, ti)) ~ c =>
        ContainsCondition(
          ListExpression(ListExpressionForm.IntRange(f, fi, t, ti, true)),
          false,
          ContainsConditionTarget.SuchThat(None, x, c),
        )
    } |
    // `for all nonnegative integers _j_ less than _n_, <cond>`
    ("for all" ~ ("nonnegative" | "non-negative") ~ "integers" ~> variable) ~
    ("less than" ~> calcExpr | "such that" ~ variable ~ "<" ~> calcExpr) ~
    ("," ~> cond) ^^ {
      case x ~ n ~ c =>
        ContainsCondition(
          ListExpression(
            ListExpressionForm.IntRange(
              DecimalMathValueLiteral(0),
              true,
              n,
              false,
              true,
            ),
          ),
          true,
          ContainsConditionTarget.SuchThat(None, x, negate(c)),
        )
    } |
    // `_x_ and _y_ are both *true* or both *false*`
    (expr <~ "and") ~ expr <~ "are both *true* or both *false*" ^^ {
      case l ~ r => IsAreCondition(List(l), false, List(r))
    } |
    // `_x_ and _y_ are exactly the same sequence of code units (...)`
    (expr <~ "and") ~ expr <~ (
      "are exactly the same sequence of code units" ~ "\\([^)]*\\)".r |
      "have the same length and the same code units in the same positions"
    ) ^^ { case l ~ r => BinaryCondition(l, SameCodeUnits, r) } |
    // `A or B or C`
    binCond ~ rep1("or" ~> binCond) ^^ {
      case l ~ rs =>
        (l :: rs).reduceRight[Condition](
          CompoundCondition(_, CompoundConditionOperator.Or, _),
        )
    }

  // ---------------------------------------------------------------------------
  // helpers
  // ---------------------------------------------------------------------------
  // end of legacy steps
  lazy val lend: Parser[Unit] =
    opt("\\([^)]*\\)".r) ~ ("." <~ upper | ";") ~ opt("\\(.*\\)".r) ^^^ ()

  // `but if there is no such integer _k_,`
  lazy val noSuchInteger: Parser[Unit] =
    "but if there is no such integer" ~ opt(variable) ~ "," ^^^ ()

  // `the smallest possible integer _k_ not smaller than _start_`
  lazy val searchBound: Parser[(Variable, Boolean, CalcExpression)] =
    ("smallest possible integer" ~> variable) ~
    ("not smaller than" ~> calcExpr) ^^ { case x ~ e => (x, true, e) } |
    ("largest possible" ~ ("nonnegative" | "non-negative") ~ "integer" ~>
    variable) ~ ("not larger than" ~> calcExpr) ^^ {
      case x ~ e => (x, false, e)
    }

  // separators
  def lsep(s: Parser[Any]): Parser[Any] = "," ~ s | s | ","

  // the arguments passed to a built-in function
  def argsList: Expression = refExpr(Variable("argumentsList"))

  def refExpr(r: Reference): ReferenceExpression = ReferenceExpression(r)
  def one: CalcExpression = DecimalMathValueLiteral(1)
  def minusOne: CalcExpression = DecimalMathValueLiteral(-1)

  // `_x_ + 1`, folding `_x_ - 1 + 1` into `_x_`
  def succ(e: CalcExpression): CalcExpression = e match
    case BinaryExpression(x, Sub, DecimalMathValueLiteral(n)) if n == 1 => x
    case _ => BinaryExpression(e, Add, one)

  // a block of desugared steps
  def block(steps: Step*): Step =
    BlockStep(StepBlock(steps.toList.map(SubStep(None, _))))

  // negation of conditions
  def negate(c: Condition): Condition =
    import BinaryConditionOperator.*
    import CompoundConditionOperator.*
    c match
      case c: IsAreCondition             => c.copy(negation = !c.negation)
      case c: PredicateCondition         => c.copy(negation = !c.negation)
      case c: ContainsCondition          => c.copy(negation = !c.negation)
      case c: InclusiveIntervalCondition => c.copy(negation = !c.negation)
      case c: TypeCheckCondition         => c.copy(negation = !c.negation)
      case c: HasFieldCondition          => c.copy(negation = !c.negation)
      case c: HasBindingCondition        => c.copy(negation = !c.negation)
      case BinaryCondition(l, op, r) =>
        val neg = op match
          case Eq               => Some(NEq)
          case NEq              => Some(Eq)
          case LessThan         => Some(GreaterThanEqual)
          case LessThanEqual    => Some(GreaterThan)
          case GreaterThan      => Some(LessThanEqual)
          case GreaterThanEqual => Some(LessThan)
          case SameCodeUnits    => None
        neg.fold(IsAreCondition(List(l), true, List(r)))(
          BinaryCondition(l, _, r),
        )
      case CompoundCondition(l, And, r) =>
        CompoundCondition(negate(l), Or, negate(r))
      case CompoundCondition(l, Or, r) =>
        CompoundCondition(negate(l), And, negate(r))
      case CompoundCondition(l, Imply, r) =>
        CompoundCondition(l, And, negate(r))
      case _ => c
}
