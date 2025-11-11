package esmeta.lang.util

import esmeta.LINE_SEP
import esmeta.lang.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for metalanguage */
class Stringifier(detail: Boolean, location: Boolean) {
  val tyStringifier = TyElem.getStringifier(false, false)

  // elements
  given elemRule: Rule[LangElem] = (app, elem) =>
    elem match {
      case elem: Syntax                     => syntaxRule(app, elem)
      case elem: PredicateConditionOperator => predCondOpRule(app, elem)
      case elem: MathFuncExpressionOperator => mathFuncExprOpRule(app, elem)
      case elem: ConversionExpressionOperator =>
        convExprOpRule(false)(app, elem)
      case elem: BinaryExpressionOperator  => binExprOpRule(app, elem)
      case elem: UnaryExpressionOperator   => unExprOpRule(app, elem)
      case elem: XRefExpressionOperator    => xrefExprOpRule(app, elem)
      case elem: BinaryConditionOperator   => binCondOpRule(app, elem)
      case elem: ContainsConditionTarget   => containsTargetRule(app, elem)
      case elem: CompoundConditionOperator => compCondOpRule(app, elem)
      case elem: MathOpExpressionOperator  => mathOpRule(app, elem)
      case elem: BitwiseExpressionOperator => bitExprOpRule(app, elem)
    }

  // syntax
  given syntaxRule: Rule[Syntax] = (app, syn) =>
    syn match {
      case syn: Block      => blockRule(app, syn)
      case syn: Step       => stepRule(app, syn)
      case syn: SubStep    => subStepRule(app, syn)
      case syn: Expression => exprRule(app, syn)
      case syn: Condition  => condRule(app, syn)
      case syn: Reference  => refRule(app, syn)
      case syn: Type       => typeRule(app, syn)
      case syn: Intrinsic  => intrRule(app, syn)
    }

  // blocks
  given blockRule: Rule[Block] = (app, block) =>
    given Rule[Step] = stepWithUpperRule(true)
    if (detail) app.wrap("", "")(block match {
      case StepBlock(steps) =>
        steps.foreach(app :> "1. " >> _)
      case ExprBlock(exprs) =>
        exprs.foreach(app :> "* " >> _)
      case Figure(lines) =>
        app :> "<figure>"
        app.wrap("", "")(lines.map(app :> _))
        app :> "</figure>"
    })
    else app >> " [...]"

  // sub-steps
  given subStepRule: Rule[SubStep] = (app, subStep) =>
    given Rule[Step] = stepWithUpperRule(true)
    val SubStep(directive, step) = subStep
    directive.map(app >> _ >> " ")
    app >> step

  given directiveRule: Rule[Directive] = (app, directive) =>
    given Rule[List[String]] = iterableRule(sep = ",")
    val Directive(name, values) = directive
    app >> "[" >> name >> "=\"" >> values >> "\"]"

  // steps
  given stepRule: Rule[Step] = stepWithUpperRule(false)
  def stepWithUpperRule(upper: Boolean): Rule[Step] = withLoc { (app, step) =>
    given Rule[First] = firstRule(upper)
    step match {
      case LetStep(x, expr) =>
        app >> First("let ") >> x >> " be " >> expr
      case SetStep(x, expr) =>
        app >> First("set ") >> x >> " to " >> expr
      case SetAsStep(x, verb, id) =>
        app >> First("set ") >> x >> " as " >> verb >> " in "
        xrefRule(app, id)
      case SetEvaluationStateStep(context, func, args) =>
        given Rule[List[Expression]] = argsRule(showNoArg = true)
        app >> First("set the code evaluation state of ") >> context
        app >> " such that when evaluation is resumed"
        app >> " for that execution context, "
        app >> func >> " will be called" >> args >> "."
      case PerformStep(expr) =>
        app >> First("perform ") >> expr
      case InvokeShorthandStep(name, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> name >> args
      case AppendStep(expr, ref) =>
        app >> First("append ") >> expr >> " to " >> ref
      case PrependStep(expr, ref) =>
        app >> First("prepend ") >> expr >> " to " >> ref
      case InsertStep(expr, ref) =>
        app >> First("insert ") >> expr >> " as the first element of " >> ref
      case AddStep(expr, ref) =>
        app >> First("add ") >> expr >> " to " >> ref
      case RemoveStep(target, prep, list) =>
        app >> First("remove ") >> target >> " " >> prep >> " " >> list
      case PushContextStep(ref) =>
        app >> First("push ") >> ref >> " onto the execution context stack;"
        app >> " " >> ref >> " is now the running execution context"
      case SuspendStep(xOpt, remove) =>
        app >> First("suspend ")
        xOpt match
          case Some(x) => app >> x
          case None    => app >> "the running execution context"
        if (remove) app >> " and remove it from the execution context stack"
      case RemoveContextStep(context, restoreTarget) =>
        app >> First("remove ") >> context
        app >> " from the execution context stack" >> restoreTarget
      case AssertStep(ExpressionCondition(YetExpression(yet, None))) =>
        app >> First("assert: ") >> yet >> "."
      case AssertStep(cond) =>
        app >> First("assert: ") >> cond
      case IfStep(cond, thenStep, elseStep, config) =>
        val IfStep.ElseConfig(newLine, keyword, comma) = config
        val k = if (thenStep.isNextLowercase) keyword else keyword.toFirstUpper
        app >> First("if ") >> cond >> ", "
        if (thenStep.isInstanceOf[BlockStep]) app >> "then"
        app >> thenStep
        elseStep.fold(app) { step =>
          if (newLine)
            step match
              case _: IfStep    => app :> "1. " >> k >> " " >> step
              case _: BlockStep => app :> "1. " >> k >> "," >> step
              case _            => app :> "1. " >> k >> ", " >> step
          else
            app >> " " >> k
            app >> (if (comma) ", " else " ") >> step
        }
      case RepeatStep(cond, body) =>
        import RepeatStep.LoopCondition.*
        app >> First("repeat,")
        cond match
          case NoCondition =>
            if (!body.isInstanceOf[BlockStep]) app >> " "
          case While(c) => app >> " " >> "while " >> c >> ","
          case Until(c) => app >> " " >> "until " >> c >> ","
        app >> body
      case ForEachStep(ty, elem, expr, forward, body) =>
        app >> First("for each ")
        given Rule[Type] = getTypeRule(ArticleOption.No)
        ty match
          case Some(ty) => app >> ty >> " "
          case None     => app >> "element "
        app >> elem >> " of " >> expr >> ", "
        if (!forward) app >> "in reverse List order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachIntegerStep(x, low, lowInc, high, highInc, ascending, body) =>
        def op(inc: Boolean): String = if (inc) " ≤ " else " < "
        app >> First("for each integer ") >> x >> " such that "
        app >> low >> op(lowInc) >> x >> op(highInc) >> high >> ", in "
        app >> (if (ascending) "ascending" else "descending") >> " order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
        import ForEachOwnPropertyKeyStepOrder.*
        app >> First("for each own property key ") >> key >> " of " >> obj
        app >> " such that " >> cond >> ", in "
        if (ascending) app >> "ascending " else app >> "descending "
        order match
          case NumericIndexOrder => app >> "numeric index order"
          case ChronologicalOrder =>
            app >> "chronological order of property creation"
        app >> ", "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachParseNodeStep(x, expr, body) =>
        app >> First("for each child node ") >> x
        app >> " of " >> expr >> ", do" >> body
      case ReturnStep(expr) =>
        app >> First("return ") >> expr
      case ThrowStep(name) =>
        app >> First("throw ")
        app >> ("*" + name + "*").withIndefArticle >> " exception"
      case ResumeStep(callerCtxt, arg, genCtxt, param, steps) =>
        given Rule[Step] = stepWithUpperRule(true)
        app >> "Resume " >> callerCtxt >> " passing " >> arg >> ". "
        app >> "If " >> genCtxt >> " is ever resumed again, "
        app >> "let " >> param >> " be "
        app >> "the Completion Record with which it is resumed."
        for (step <- steps) app :> "1. " >> step
        app
      case ResumeEvaluationStep(context, argOpt, paramOpt, body) =>
        given Rule[Step] = stepWithUpperRule(true)
        effect(app) {
          app >> "Resume the suspended evaluation of " >> context
        }
        for (arg <- argOpt)
          app >> " using " >> arg
          app >> " as the result of the operation that suspended it"
        app >> "."
        for ((param, kind) <- paramOpt)
          app >> " Let " >> param
          app >> " be the " >> kind >> " returned by the resumed computation."
        for (step <- body) app :> "1. " >> step
        app
      case ResumeTopContextStep() =>
        app >> "Resume the context that is now on the top of the "
        app >> "execution context stack as the running execution context."
      case NoteStep(note) =>
        app >> "NOTE: " >> note
      case BlockStep(block) =>
        app >> block
      case YetStep(expr) =>
        app >> expr
      // -----------------------------------------------------------------------
      // special steps rarely used in the spec
      // -----------------------------------------------------------------------
      case SetFieldsWithIntrinsicsStep(ref, desc) =>
        app >> First("set fields of ") >> ref >> " with the values listed in "
        xrefRule(app, "table-well-known-intrinsic-objects") >> "."
        app >> " " >> desc
      case PerformBlockStep(block, desc) =>
        app >> First("perform ")
        app >> "the following substeps in an implementation-defined order"
        if (desc.nonEmpty) app >> ", " >> desc
        app >> ":" >> block
    }
    app >> step.endString
  }

  given removeStepTargetRule: Rule[RemoveStep.Target] = (app, target) => {
    import RemoveStep.Target
    given Rule[Option[Expression]] = (app, expr) =>
      expr match
        case Some(e) => app >> e >> " elements"
        case None    => app >> "element"
    target match
      case Target.First(count)  => app >> "the first " >> count
      case Target.Last(count)   => app >> "the last " >> count
      case Target.Element(expr) => app >> expr
  }

  given removeCtxtStepRestoreTargetRule: Rule[RemoveContextStep.RestoreTarget] =
    (app, target) => {
      import RemoveContextStep.RestoreTarget.*
      target match
        case NoRestore => app
        case StackTop =>
          app >> " and restore the execution context that is "
          app >> "at the top of the execution context stack"
          app >> " as the running execution context"
        case Context(ref) =>
          app >> " and restore " >> ref >> " as the running execution context"
    }

  // ForEachOwnPropertyKeyStepOrder
  given ForEachOwnPropertyKeyStepOrderRule
    : Rule[ForEachOwnPropertyKeyStepOrder] =
    (app, order) =>
      import ForEachOwnPropertyKeyStepOrder.*
      app >> (order match
        case NumericIndexOrder  => "numeric index order"
        case ChronologicalOrder => "chronological order of property creation"
      )
  private case class First(str: String)
  private def firstRule(upper: Boolean): Rule[First] = (app, first) => {
    val First(str) = first
    app >> (if (upper) str.toFirstUpper else str)
  }

  // expressions
  given exprRule: Rule[Expression] = withLoc { (app, expr) =>
    expr match {
      case StringConcatExpression(exprs) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        app >> "the string-concatenation of " >> exprs
      case ListConcatExpression(exprs) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        app >> "the list-concatenation of " >> exprs
      case ListCopyExpression(expr) =>
        app >> "a List whose elements are the elements of " >> expr
      case RecordExpression(ty, fields, form) =>
        import RecordExpressionForm.*
        form match {
          case SyntaxLiteral(prefix) =>
            given Rule[(FieldLiteral, Expression)] = {
              case (app, (field, expr)) =>
                app >> field >> ": " >> expr
            }
            given Rule[List[(FieldLiteral, Expression)]] =
              iterableRule("{ ", ", ", " }")
            app >> prefix.fold("")(_ + " ") >> ty >> " "
            if (fields.isEmpty) app >> "{ }"
            else app >> fields
          case Text =>
            val (field, expr) = fields.head
            app >> "a new " >> ty >> " whose " >> field >> " is " >> expr
          case TextWithNoElement(prefix, postfix) =>
            app >> prefix >> " " >> ty >> postfix.fold("")(" " + _)
        }
      case LengthExpression(expr) =>
        app >> "the length of " >> expr
      case SubstringExpression(expr, from, to) =>
        app >> "the substring of " >> expr >> " from " >> from
        to.fold(app)(app >> " to " >> _)
      case TrimExpression(expr, leading, trailing) =>
        app >> "the String value that is a copy of " >> expr >> " with "
        app >> ((leading, trailing) match
          case (true, true)   => "both leading and trailing"
          case (true, false)  => "leading"
          case (false, true)  => "trailing"
          case (false, false) => "no"
        )
        app >> " white space removed"
      case NumberOfExpression(name, pre, expr, exclude) =>
        app >> "the number of " >> name >> " in "
        pre.map(app >> "the " >> _ >> " ")
        app >> expr
        exclude.fold(app)(app >> ", excluding all occurrences of " >> _)
      case SourceTextExpression(expr) =>
        app >> "the source text matched by " >> expr
      case CoveredByExpression(code, rule) =>
        app >> "the " >> rule >> " that is covered by " >> code
      case GetItemsExpression(nt, expr) =>
        app >> "the List of " >> nt >> " items in " >> expr
        app >> ", in source text order"
      case IntrinsicExpression(intr) =>
        app >> intr
      case XRefExpression(op, id) =>
        import XRefExpressionOperator.*
        val o = op match
          case Algo       => "the algorithm steps defined in"
          case Definition => "the definition specified in"
          case InternalMethod =>
            "the ordinary object internal method defined in"
          case InternalSlots => "the internal slots listed in"
          case ParamLength =>
            "the number of non-optional parameters of the function definition in"
        xrefRule(app >> o >> " ", id)
      case expr: CalcExpression =>
        calcExprRule(app, expr)
      case ClampExpression(target, lower, upper) =>
        app >> "the result of clamping " >> target >> " between " >> lower
        app >> " and " >> upper
      case expr: MathOpExpression => mathOpExprRule(app, expr)
      case BitwiseExpression(left, op, right) =>
        app >> "the result of applying the " >> op >> " to " >> left
        app >> " and " >> right
      case expr: InvokeExpression =>
        invokeExprRule(app, expr)
      case ListExpression(form) =>
        import ListExpressionForm.*
        form match
          case LiteralSyntax(entries) =>
            entries match
              case Nil => app >> "« »"
              case _ =>
                given Rule[Iterable[Expression]] =
                  iterableRule("« ", ", ", " »")
                app >> entries
          case SoleElement(e) =>
            app >> "a List whose sole element is " >> e
          case EmptyList(isNewUsed, typeDesc) =>
            if (isNewUsed) app >> "a new empty List"
            else app >> s"an empty List"
            typeDesc.fold(app)(app >> " of " >> _)
          case IntRange(from, isFromInc, to, isToInc, isInc) =>
            app >> "a List of the integers in the interval from " >> from
            app >> " (" >> (if (isFromInc) "inclusive" else "exclusive") >> ")"
            app >> " to " >> to
            app >> " (" >> (if (isToInc) "inclusive" else "exclusive") >> ")"
            app >> ", in " >> (if (isInc) "ascending"
                               else "descending") >> " order"
      case SoleElementExpression(expr) =>
        app >> "the sole element of " >> expr
      case CodeUnitAtExpression(base, index) =>
        app >> "the code unit at index " >> index >> " within " >> base
      case StringExpression(expr) =>
        app >> "the String value " >> expr
      case YetExpression(str, block) =>
        app >> str
        block.fold(app)(app >> _)
      case multi: MultilineExpression => app >> multi
    }
  }

  // mathematical operations
  lazy val mathOpExprRule: Rule[MathOpExpression] = (app, expr) =>
    import MathOpExpressionOperator.*
    val MathOpExpression(op, args) = expr
    app >> "the " >> op >> " "
    (op, args) match
      case (Neg, List(e)) =>
        app >> e
      case (Add, List(l, r)) =>
        app >> l >> " and " >> r
      case (Mul, List(l, r)) =>
        app >> l >> " and " >> r
      case (Sub, List(l, r)) =>
        app >> l >> " minus " >> r
      case (Pow, List(l, r)) =>
        app >> l >> " to the " >> r >> " power"
      case (Expm1, List(e)) =>
        app >> e
      case (Log10, List(e)) =>
        app >> e
      case (Log2, List(e)) =>
        app >> e
      case (Cos, List(e)) =>
        app >> e
      case (Cbrt, List(e)) =>
        app >> e
      case (Exp, List(e)) =>
        app >> e
      case (Cosh, List(e)) =>
        app >> e
      case (Sinh, List(e)) =>
        app >> e
      case (Tanh, List(e)) =>
        app >> e
      case (Acos, List(e)) =>
        app >> e
      case (Acosh, List(e)) =>
        app >> e
      case (Asinh, List(e)) =>
        app >> e
      case (Atanh, List(e)) =>
        app >> e
      case (Asin, List(e)) =>
        app >> e
      case (Atan2, List(x, y)) =>
        app >> x >> " / " >> y
      case (Atan, List(e)) =>
        app >> e
      case (Log1p, List(e)) =>
        app >> e
      case (Log, List(e)) =>
        app >> e
      case (Sin, List(e)) =>
        app >> e
      case (Sqrt, List(e)) =>
        app >> e
      case (Tan, List(e)) =>
        app >> e
      case _ => raise(s"invalid math operationr: $op with $args")

  // multiline expressions
  given multilineExprRule: Rule[MultilineExpression] = (app, expr) =>
    expr match {
      case AbstractClosureExpression(params, captured, body) =>
        given Rule[List[Variable]] = listNamedSepRule(namedSep = "and")
        app >> "a new Abstract Closure with"
        if (params.isEmpty) app >> " no parameters "
        else {
          given Rule[List[Variable]] = iterableRule(sep = ", ")
          app >> " parameters (" >> params >> ") "
        }
        app >> "that captures "
        if (captured.isEmpty) app >> "nothing"
        else app >> captured
        app >> " and performs the following steps when called:" >> body
    }

  // calculation expressions
  given calcExprRule: Rule[CalcExpression] = calcExprRuleWithLevel(0)

  def calcExprRuleWithLevel(level: Int): Rule[CalcExpression] = (app, expr) =>
    import ConversionExpressionForm.*
    given Rule[CalcExpression] = calcExprRuleWithLevel(expr.level)
    if (expr.level < level) app >> "("
    expr match {
      case ReturnIfAbruptExpression(expr, check) =>
        app >> (if (check) "?" else "!") >> " " >> expr
      case ReferenceExpression(ref) =>
        app >> ref
      case MathFuncExpression(op, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> op >> args
      case ConversionExpression(o, expr, SyntaxLiteral) =>
        given Rule[ConversionExpressionOperator] = convExprOpRule(text = false)
        app >> o >> "(" >> expr >> ")"
      case ConversionExpression(op, expr, Text(a, pre)) =>
        given Rule[ConversionExpressionOperator] = convExprOpRule(text = true)
        app >> a >> " " >> op >> " value " >> pre >> " " >> expr
      case ExponentiationExpression(base, power) =>
        app >> base >> "<sup>" >> power >> "</sup>"
      case BinaryExpression(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case UnaryExpression(op, expr) =>
        app >> op >> expr
      case lit: Literal =>
        litRule(app, lit)
    }
    if (expr.level < level) app >> ")"
    app

  // operators for mathematical function expressions
  given mathFuncExprOpRule: Rule[MathFuncExpressionOperator] = (app, op) =>
    import MathFuncExpressionOperator.*
    app >> (op match {
      case Max      => "max"
      case Min      => "min"
      case Abs      => "abs"
      case Floor    => "floor"
      case Truncate => "truncate"
    })

  // operators for conversion operation expressions
  def convExprOpRule(text: Boolean): Rule[ConversionExpressionOperator] =
    (app, op) =>
      import ConversionExpressionOperator.*
      app >> (op match {
        case ToApproxNumber => "implementation-approximated Number"
        case ToNumber       => if (text) "Number" else "𝔽"
        case ToBigInt       => if (text) "BigInt" else "ℤ"
        case ToMath         => if (text) "numeric" else "ℝ"
        case ToCodeUnit     => "code unit whose numeric value"
      })

  // operators for binary expressions
  given binExprOpRule: Rule[BinaryExpressionOperator] = (app, op) =>
    import BinaryExpressionOperator.*
    app >> (op match {
      case Add => "+"
      case Sub => "-"
      case Mul => "×"
      case Div => "/"
      case Mod => "modulo"
    })

  // operators for unary expressions
  given unExprOpRule: Rule[UnaryExpressionOperator] = (app, op) =>
    import UnaryExpressionOperator.*
    app >> (op match {
      case Neg => "-"
    })

  // operators for emu-xref expressions
  given xrefExprOpRule: Rule[XRefExpressionOperator] = (app, op) =>
    import XRefExpressionOperator.*
    app >> (op match {
      case Definition => "the definition specified in"
      case Algo       => "the algorithm steps defined in"
      case InternalMethod =>
        "the ordinary object internal method defined in"
      case InternalSlots => "the internal slots listed in"
      case ParamLength =>
        "the number of non-optional parameters of the function definition in"
    })

  // literals
  given litRule: Rule[Literal] = (app, lit) =>
    lit match {
      case ThisLiteral(article) =>
        val a = if (article) "the " else ""
        app >> a >> "*this* value"
      case ThisParseNodeLiteral(nt) =>
        nt match {
          case None     => app >> "this Parse Node"
          case Some(nt) => app >> "this" >> " " >> nt
        }
      case _: NewTargetLiteral => app >> "NewTarget"
      case HexLiteral(hex, codeUnitDesc, isUnicodePrefix, name) =>
        if (codeUnitDesc) app >> "the code unit "
        app >> (if (isUnicodePrefix) "U+" else "0x")
        app >> f"$hex%04X"
        name.map(app >> " (" >> _ >> ")")
        app
      case CodeLiteral(code) => app >> "`" >> code >> "`"
      case GrammarSymbolLiteral(name, flags) =>
        given Rule[Iterable[String]] = iterableRule("[", ", ", "]")
        app >> "the grammar symbol "
        app >> "|" >> name
        if (!flags.isEmpty) app >> flags
        app >> "|"
      case NonterminalLiteral(ordinal, name, flags, article) =>
        given Rule[Iterable[String]] = iterableRule("[", ", ", "]")
        if (article) app >> "the "
        ordinal.map(ord => app >> ord.toOrdinal >> " ")
        app >> "|" >> name
        if (!flags.isEmpty) app >> flags
        app >> "|"
      case EnumLiteral(name) => app >> "~" >> name >> "~"
      case StringLiteral(str, form) =>
        import StringLiteralForm.*
        form match {
          case SyntaxLiteral =>
            val replaced = str
              .replace("\\", "\\\\")
              .replace("*", "\\*")
            app >> "*\"" >> replaced >> "\"*"
          case EmptyString => app >> "the empty String"
          case EmptyUnicode =>
            app >> "the empty sequence of Unicode code points"
          case Code => app >> "<code>\"" >> str >> "\"</code>"
        }
      case FieldLiteral(name) => app >> "[[" >> name >> "]]"
      case SymbolLiteral(sym) => app >> "%Symbol." >> sym >> "%"
      case ProductionLiteral(lhs, rhs) =>
        app >> "<emu-grammar>" >> lhs >> " : " >> rhs >> "</emu-grammar>"
      case ErrorObjectLiteral(name) =>
        app >> "a newly created *" >> name >> "* object"
      case _: PositiveInfinityMathValueLiteral => app >> "+∞"
      case _: NegativeInfinityMathValueLiteral => app >> "-∞"
      case DecimalMathValueLiteral(n)          => app >> n
      case MathConstantLiteral(pre, name) =>
        if (pre != 1) app >> pre
        app >> name
      case NumberLiteral(n) =>
        if (n.isNaN) app >> "*NaN*"
        else
          app >> "*" >> (
            if (n.isPosInfinity) "+∞"
            else if (n.isNegInfinity) "-∞"
            else if (n == 0) if ((1 / n).isPosInfinity) "+0" else "-0"
            else if (n.toInt == n) n.toInt.toString
            else n.toString
          ) >> "*<sub>𝔽</sub>"
      case BigIntLiteral(n)        => app >> "*" >> n >> "*<sub>ℤ</sub>"
      case _: TrueLiteral          => app >> "*true*"
      case _: FalseLiteral         => app >> "*false*"
      case _: UndefinedLiteral     => app >> "*undefined*"
      case _: NullLiteral          => app >> "*null*"
      case _: UndefinedTypeLiteral => app >> "Undefined"
      case _: NullTypeLiteral      => app >> "Null"
      case _: BooleanTypeLiteral   => app >> "Boolean"
      case _: StringTypeLiteral    => app >> "String"
      case _: SymbolTypeLiteral    => app >> "Symbol"
      case _: NumberTypeLiteral    => app >> "Number"
      case _: BigIntTypeLiteral    => app >> "BigInt"
      case _: ObjectTypeLiteral    => app >> "Object"
    }

  // operators for bitwise expressions
  given bitExprOpRule: Rule[BitwiseExpressionOperator] = (app, op) =>
    import BitwiseExpressionOperator.*
    app >> (op match {
      case BAnd => "bitwise AND operation"
      case BXOr => "bitwise exclusive OR (XOR) operation"
      case BOr  => "bitwise inclusive OR operation"
    })

  // metalanguage invocation expressions
  given invokeExprRule: Rule[InvokeExpression] = (app, expr) =>
    expr match {
      case InvokeAbstractOperationExpression(name, args, tag) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        tagEffect(app, tag)(app >> name, app >> args)
      case InvokeNumericMethodExpression(base, name, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> base >> "::" >> name >> args
      case InvokeAbstractClosureExpression(x, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> x >> args
      case InvokeMethodExpression(access, args, tag) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        tagEffect(app, tag)(app >> access, app >> args)
      case InvokeSyntaxDirectedOperationExpression(
            base,
            name,
            args,
            article,
            tag,
          ) =>
        // XXX handle Contains, Contains always takes one argument
        if (name == "Contains") app >> base >> " Contains " >> args.head
        // Otherwise
        else {
          given Rule[List[Expression]] = argsRule(showNoArg = false)
          val a = article match {
            case None        => ""
            case Some(value) => s"$value "
          }
          tagEffect(app, tag)(app >> a >> name >> " of " >> base, app >> args)
        }
        app
    }

  def argsRule(showNoArg: Boolean): Rule[List[Expression]] = (app, args) => {
    given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
    if (!args.isEmpty)
      app >> " with argument" >> (if (args.length > 1) "s " else " ")
      app >> args
    else if (showNoArg) app >> " with no arguments"
    else app
  }

  // conditions
  given condRule: Rule[Condition] = withLoc { (app, cond) =>
    cond match {
      case ExpressionCondition(expr) =>
        app >> expr
      case TypeCheckCondition(expr, neg, ty) =>
        app >> expr
        ty match {
          case t :: Nil => app >> isStr(neg) >> t
          case _ =>
            app >> " is "
            val (left, namedSep) =
              if (neg) ("neither ", "nor") else ("either ", "or")
            given Rule[List[Type]] = listNamedSepRule(left, namedSep)
            app >> ty
        }
      case HasFieldCondition(ref, neg, field, form, tyOpt) =>
        app >> ref >> hasStr(neg)
        if (field.length > 1)
          given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
          app >> field >> " "
        else
          app >> field.head.toString.indefArticle
          app >> " " >> field.head >> " "
        import HasFieldConditionForm.*
        app >> (form match {
          case Field          => "field"
          case InternalSlot   => "internal slot"
          case InternalMethod => "internal method"
        })
        if (field.length > 1) app >> "s"
        tyOpt.fold(app)(app >> " whose value is " >> _)
      case HasBindingCondition(ref, neg, binding) =>
        app >> ref >> hasStr(neg)
        app >> "a binding for " >> binding
      case ProductionCondition(nt, lhs, rhs) =>
        app >> nt >> " is " >> "<emu-grammar>"
        app >> lhs >> " : " >> rhs
        app >> "</emu-grammar>"
      case PredicateCondition(x, neg, op) =>
        // TODO is/has
        app >> x >> isStr(neg) >> op
      case IsAreCondition(ls, neg, rs) =>
        val single = ls.length == 1
        if (single) app >> ls.head
        else {
          given Rule[List[Expression]] = listNamedSepRule("both ", "and")
          app >> ls
        }
        rs match {
          case r :: Nil => app >> isStr(neg, single) >> r
          case _ =>
            app >> isStr(false, single)
            if (neg) {
              given Rule[List[Expression]] =
                listNamedSepRule(namedSep = "nor")
              app >> "neither " >> rs
            } else {
              given Rule[List[Expression]] =
                listNamedSepRule(namedSep = "or")
              app >> "either " >> rs
            }
        }
      case BinaryCondition(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case InclusiveIntervalCondition(left, neg, from, to, verbose) =>
        if (verbose)
          app >> left >> " is"
          if (neg) app >> " not"
          app >> " in the inclusive interval from " >> from
          app >> " to " >> to
        else app >> from >> " ≤ " >> left >> " ≤ " >> to
      case ContainsCondition(list, neg, expr) =>
        app >> list >> (if (neg) " does not contain " else " contains ") >> expr
      case CompoundCondition(left, CompoundConditionOperator.Imply, right) =>
        app >> "If " >> left >> ", then " >> right
      case CompoundCondition(left, op, right) =>
        // collect sub conditions of same level in a single list
        def flatten(cond: Condition): List[Condition] = {
          cond match {
            case CompoundCondition(l, o, r) if o == op => l :: flatten(r)
            case _                                     => List(cond)
          }
        }
        val conds = flatten(cond)
        val sep = op match {
          case CompoundConditionOperator.And   => "and"
          case CompoundConditionOperator.Or    => "or"
          case CompoundConditionOperator.Imply => "then" // not used
        }
        given Rule[List[Condition]] = listNamedSepRule(namedSep = sep)
        app >> conds
    }
  }

  // operators for predicate conditions
  given predCondOpRule: Rule[PredicateConditionOperator] = (app, op) =>
    import PredicateConditionOperator.*
    app >> (op match {
      case Finite           => "finite"
      case Abrupt           => "an abrupt completion"
      case Throw            => "a throw completion"
      case Return           => "a return completion"
      case Break            => "a break completion"
      case Continue         => "a continue completion"
      case NeverAbrupt      => "never an abrupt completion"
      case Normal           => "a normal completion"
      case Duplicated       => "duplicate entries"
      case Present          => "present"
      case Empty            => "empty"
      case StrictMode       => "strict mode code"
      case ArrayIndex       => "an array index"
      case FalseToken       => "the token `false`"
      case TrueToken        => "the token `true`"
      case DataProperty     => "a data property"
      case AccessorProperty => "an accessor property"
      case FullyPopulated   => "a fully populated Property Descriptor"
      case Nonterminal      => "an instance of a nonterminal"
    })

  // operators for binary conditions
  given binCondOpRule: Rule[BinaryConditionOperator] = (app, op) =>
    import BinaryConditionOperator.*
    app >> (op match {
      case Eq               => "="
      case NEq              => "≠"
      case LessThan         => "<"
      case LessThanEqual    => "≤"
      case GreaterThan      => ">"
      case GreaterThanEqual => "≥"
      case SameCodeUnits    => "is the same sequence of code units as"
    })

  // targets for `contains` conditions
  given containsTargetRule: Rule[ContainsConditionTarget] = (app, target) => {
    import ContainsConditionTarget.*
    given Rule[Option[Type]] = (app, tyOpt) =>
      tyOpt match
        case Some(ty) => app >> ty
        case None     => app >> "an element"
    target match
      case Expr(e) => app >> e
      case WhoseField(tyOpt, field, expr) =>
        app >> tyOpt >> " whose [[" >> field >> "]] is " >> expr
      case SuchThat(tyOpt, x, cond) =>
        app >> tyOpt >> " " >> x >> " such that " >> cond
  }

  // operators for compound conditions
  given compCondOpRule: Rule[CompoundConditionOperator] = (app, op) => {
    import CompoundConditionOperator.*
    app >> (op match {
      case And   => "and"
      case Or    => "or"
      case Imply => "then" // XXX not used
    })
  }

  // operators for mathematical operations
  given mathOpRule: Rule[MathOpExpressionOperator] = (app, op) => {
    import MathOpExpressionOperator.*
    app >> (op match
      case Neg   => "negation of"
      case Add   => "sum of"
      case Mul   => "product of"
      case Sub   => "difference"
      case Pow   => "raising"
      case Expm1 => "subtracting 1 from the exponential function of"
      case Log10 => "base 10 logarithm of"
      case Log2  => "base 2 logarithm of"
      case Cos   => "cosine of"
      case Cbrt  => "cube root of"
      case Exp   => "exponential function of"
      case Cosh  => "hyperbolic cosine of"
      case Sinh  => "hyperbolic sine of"
      case Tanh  => "hyperbolic tangent of"
      case Acos  => "inverse cosine of"
      case Acosh => "inverse hyperbolic cosine of"
      case Asinh => "inverse hyperbolic sine of"
      case Atanh => "inverse hyperbolic tangent of"
      case Asin  => "inverse sine of"
      case Atan2 => "inverse tangent of the quotient"
      case Atan  => "inverse tangent of"
      case Log1p => "natural logarithm of 1 +"
      case Log   => "natural logarithm of"
      case Sin   => "sine of"
      case Sqrt  => "square root of"
      case Tan   => "tangent of"
    )
  }

  // references
  given refRule: Rule[Reference] = withLoc { (app, ref) =>
    given Rule[(String, AccessKind)] = (app, pair) => {
      import AccessKind.*
      val (name, kind) = pair
      kind match
        case Field           => app >> "[[" >> name >> "]]"
        case Component(post) => app >> name >> (if (post) " component" else "")
    }
    ref match {
      case Variable(name, nt) =>
        nt.fold(app)(app >> "|" >> _ >> "| ") >> "_" >> name >> "_"
      case Access(base, name, kind, AccessForm.Dot) =>
        app >> base >> "." >> (name, kind)
      case Access(base, name, kind, AccessForm.Of) =>
        app >> "the " >> (name, kind) >> " of " >> base
      case Access(base, name, kind, AccessForm.Apo(desc)) =>
        app >> base >> "'s " >> (name, kind)
        desc.fold(app)(app >> " " >> _)
      case ValueOf(base) =>
        app >> "the value of " >> base
      case IntrinsicField(base, intr) =>
        app >> base >> "." >> "[[" >> intr >> "]]"
      case IndexLookup(base, index) =>
        app >> base >> "[" >> index >> "]"
      case BindingLookup(base, binding) =>
        app >> "the binding for " >> binding >> " in " >> base
      case NonterminalLookup(base, nt) =>
        app >> "the |" >> nt >> "| of " >> base
      case PositionalElement(base, isFirst) =>
        if (isFirst) app >> "the first element of " >> base
        else app >> "the last element of " >> base
      case IntrinsicObject(base, expr) =>
        app >> base >> "'s intrinsic object named " >> expr
      case _: RunningExecutionContext =>
        app >> "the running execution context"
      case _: SecondExecutionContext =>
        app >> "the second to top element of the execution context stack"
      case _: CurrentRealmRecord =>
        app >> "the current Realm Record"
      case _: ActiveFunctionObject =>
        app >> "the active function object"
      case AgentRecord() =>
        app >> "the Agent Record of the surrounding agent"
    }
  }

  // intrinsics
  given intrRule: Rule[Intrinsic] = (app, intr) =>
    val Intrinsic(base, props) = intr
    app >> "%" >> base
    props.map(app >> "." >> _)
    app >> "%"

  // types
  given typeRule: Rule[Type] = getTypeRule(ArticleOption.Single)

  def getTypeRule(article: ArticleOption): Rule[Type] = (app, ty) =>
    given Rule[Ty] = tyStringifier.tyRule
    ty.ty match
      case UnknownTy(msg) => app >> msg.getOrElse("unknown")
      case ty: ValueTy    => valueTyRule(article, false)(app, ty)

  // predefined types
  private lazy val predTys: List[(ValueTy, String)] = List(
    ESValueT -> "ECMAScript language value",
  )

  // value types
  def valueTyRule(
    article: ArticleOption,
    withEither: Boolean,
  ): Rule[ValueTy] = (app, originTy) => {
    import ArticleOption.*
    var ty: ValueTy = originTy
    var tys: Vector[String] = Vector()

    // predefined types
    for ((pred, name) <- predTys if pred <= ty)
      tys :+= name.withArticle(article); ty --= pred

    // named records
    ty.record match {
      case RecordTy.Top => app >> "Record".withArticle(article)
      case recordTy @ RecordTy.Elem(map) => {
        given Rule[ValueTy] = valueTyRule(article, true)
        var m = map
        if (CompT.record <= recordTy)
          m -= "CompletionRecord"
          app >> "completion record"
        val both = m.contains("NormalCompletion") && (
          m.contains("AbruptCompletion") ||
          m.contains("BreakCompletion") ||
          m.contains("ContinueCompletion") ||
          m.contains("ReturnCompletion") ||
          m.contains("ThrowCompletion")
        )
        if (both) app >> "either "
        val normalStr = map.get("NormalCompletion").fold("") { fm =>
          m -= "NormalCompletion"
          val app = new Appender
          app >> "normal completion".withArticle(article)
          if (fm.map.keySet == Set("Value")) app >> " containing "
          app >> fm("Value").value
          app.toString
        }
        app >> normalStr
        if (both) app >> (if (normalStr contains " or ") ", or " else " or ")
        map.get("AbruptCompletion").map { fm =>
          m -= "AbruptCompletion"
          fm("Type").value.enumv match
            case Fin(set) if set.nonEmpty =>
              given Rule[Iterable[String]] = listNamedSepRule(namedSep = "or")
              app >> set.toList.sorted
                .map(x => s"$x completion".withArticle(article))
              println(app)
            case _ => app >> "abrupt completion".withArticle(article)
        }
        map.get("BreakCompletion").map { fm =>
          m -= "BreakCompletion"
          tys :+= "break completion".withArticle(article)
        }
        map.get("ContinueCompletion").map { fm =>
          m -= "ContinueCompletion"
          tys :+= "continue completion".withArticle(article)
        }
        map.get("ReturnCompletion").map { fm =>
          m -= "ReturnCompletion"
          tys :+= "return completion".withArticle(article)
        }
        map.get("ThrowCompletion").map { fm =>
          m -= "ThrowCompletion"
          tys :+= "throw completion".withArticle(article)
        }
        for (fm <- m.get("Object")) {
          if (fm == FieldMap.init("Call"))
            m -= "Object"
            tys :+= "function object".withArticle(article)
          if (fm == FieldMap.init("Call", "Construct"))
            m -= "Object"
            tys :+= "constructor".withArticle(article)
        }

        if (!map.isEmpty && m.forall((name, _) => name.isEmpty))
          // unnamed records
          for ((_, field) <- m)
            val fieldStr =
              field.map.keys.map("[[" + _ + "]]").mkString(" { ", ", ", " }")
            tys :+= ("Record" + fieldStr).withArticle(article)
        else
          // other named records
          for ((name, _) <- m) {
            // split camel case with a space
            tys :+= name.split("(?=[A-Z])").mkString(" ").withArticle(article)
          }
      }
    }

    // TODO more precise
    // closures
    if (!ty.clo.isBottom) tys :+= "Abstract Closure".withArticle(article)

    // math values
    if (ty.math == MathTy.Int)
      tys :+= "integer".withArticle(article)
    else if (!ty.math.isBottom)
      tys :+= "math value".withArticle(article)

    // TODO more precise
    // grammar symbol
    if (!ty.grammarSymbol.isBottom)
      tys :+= "grammar symbol".withArticle(article)

    // lists
    ty.list match
      case ListTy.Top => tys :+= "a List"
      case ListTy.Elem(ty) =>
        val sub = valueTyRule(Plural, true)(new Appender, ty)
        tys :+= s"a List of $sub"
      case _ =>

    // AST values
    ty.ast.names match
      case Inf => tys :+= "Parse Node".withArticle(article)
      case Fin(set) =>
        for (name <- set.toList.sorted)
          tys :+= s"|$name| Parse Node".withArticle(article)

    // TODO more precise
    // enums
    ty.enumv match
      case Inf => tys :+= "enum value".withArticle(article)
      case Fin(set) =>
        for (name <- set.toList.sorted) tys :+= s"~$name~"

    // numbers
    if (ty.number == NumberTy.Int)
      tys :+= "integral Number".withArticle(article)
    else if (ty.number == NumberTy.NaN)
      tys :+= "NaN".withArticle(article)
    else if (!ty.number.isBottom) tys :+= "Number".withArticle(article)

    // big integers
    if (!ty.bigInt.isBottom) tys :+= "BigInt".withArticle(article)

    // strings
    ty.str match
      case Inf      => tys :+= "String".withArticle(article)
      case Fin(set) => for (s <- set.toList.sorted) tys :+= s"\"$s\""

    // booleans
    if (ty.bool.set.size > 1) tys :+= "Boolean".withArticle(article)
    else if (ty.bool.set.size == 1) tys :+= s"*${ty.bool.set.head}*"

    // undefined
    if (!ty.undef.isBottom) tys :+= "*undefined*"

    // null
    if (!ty.nullv.isBottom) tys :+= "*null*"

    given Rule[Iterable[String]] = listNamedSepRule(namedSep = "or")
    if (withEither && tys.length >= 2) app >> "either "
    app >> tys
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // list with named separator
  private def listNamedSepRule[T](
    left: String = "",
    namedSep: String = "",
    right: String = "",
  )(using tRule: Rule[T]): Rule[Iterable[T]] = (app, list) =>
    val len = list.size
    app >> left
    for ((x, k) <- list.zipWithIndex) app >> (k match {
      case 0                 => ""
      case _ if k == len - 1 => (if (len > 2) ", " else " ") + namedSep + " "
      case _                 => ", "
    }) >> x
    app >> right

  // append locations
  private def withLoc[T <: Locational](rule: Rule[T]): Rule[T] = (app, elem) =>
    given Rule[Option[Loc]] = (app, locOpt) =>
      locOpt.fold(app)(app >> " @ " >> _.toString)
    rule(app, elem)
    if (location) app >> elem.loc else app

  // verbs
  private def isStr(neg: Boolean, single: Boolean = true): String =
    val res = if (single) " is " else " are "
    if (neg) res + "not " else res
  private def hasStr(neg: Boolean): String =
    if (neg) " does not have " else " has "

  // xref
  private lazy val xrefRule: Rule[String] = (app, id) =>
    app >> "<emu-xref href=\"#" >> id >> "\"></emu-xref>"

  // user-code effects
  def effect(app: Appender, c: String = "effects")(body: => Unit): Appender = {
    app >> "<emu-meta " >> c >> "=\"user-code\">"
    body
    app >> "</emu-meta>"
  }

  def tagEffect(
    app: Appender,
    tag: HtmlTag,
  )(before: => Appender, after: => Appender): Appender = {
    tag match
      case HtmlTag.None =>
        before
        after
      case HtmlTag.BeforeCall(c) =>
        effect(app, c) {
          before
        }
        after
      case HtmlTag.AfterCall(c) =>
        effect(app, c) {
          before
          after
        }
  }
}
