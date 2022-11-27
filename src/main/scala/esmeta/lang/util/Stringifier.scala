package esmeta.lang.util

import esmeta.LINE_SEP
import esmeta.lang.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for metalanguage */
class Stringifier(detail: Boolean, location: Boolean) {
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
      case elem: CompoundConditionOperator => compCondOpRule(app, elem)
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
      case syn: Property   => propRule(app, syn)
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
        given Rule[Expression] = endWithExprRule
        app >> First("let ") >> x >> " be " >> expr
      case SetStep(x, expr) =>
        given Rule[Expression] = endWithExprRule
        app >> First("set ") >> x >> " to " >> expr
      case SetFieldsWithIntrinsicsStep(ref) =>
        app >> First("set fields of ") >> ref >> " with the values listed in "
        app >> "<emu-xref href=\"#table-well-known-intrinsic-objects\">"
        app >> "</emu-xref>."
      case IfStep(cond, thenStep, elseStep) =>
        app >> First("if ") >> cond >> ", "
        if (thenStep.isInstanceOf[BlockStep]) app >> "then"
        app >> thenStep
        elseStep match {
          case Some(ifStep: IfStep)  => app :> "1. Else " >> ifStep
          case Some(step: BlockStep) => app :> "1. Else," >> step
          case Some(step)            => app :> "1. Else, " >> step
          case None                  => app
        }
      case ReturnStep(expr) =>
        given Rule[Expression] = endWithExprRule
        app >> First("return")
        expr match {
          case None    => app >> "."
          case Some(e) => app >> " " >> e
        }
      case AssertStep(cond) =>
        app >> First("assert: ") >> cond >> "."
      case ForEachStep(ty, elem, expr, ascending, body) =>
        app >> First("for each ")
        ty.map(app >> _ >> " ")
        app >> elem >> " of " >> expr >> ", "
        if (!ascending) app >> "in reverse List order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachIntegerStep(x, start, cond, ascending, body) =>
        app >> First("for each integer ") >> x >> " starting with " >> start
        app >> " such that " >> cond >> ", in "
        app >> (if (ascending) "ascending" else "descending") >> " order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachArrayIndexStep(key, array, start, ascending, body) =>
        app >> First("for each own property key ") >> key >> " of " >> array
        app >> " that is an array index,"
        app >> " whose numeric value is greater than or equal to "
        app >> start >> ", " >> "in descending numeric index order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachParseNodeStep(x, expr, body) =>
        app >> First("for each child node ") >> x
        app >> " of " >> expr >> ", do" >> body
      case ThrowStep(expr) =>
        app >> First("throw ") >> expr >> "."
      case PerformStep(expr) =>
        app >> First("perform ") >> expr >> "."
      case PerformBlockStep(block) =>
        app >> First("perform ")
        app >> "the following substeps in an implementation-defined order:"
        app >> block
      case AppendStep(expr, ref) =>
        app >> First("append ") >> expr >> " to " >> ref >> "."
      case PrependStep(expr, ref) =>
        app >> First("prepend ") >> expr >> " to " >> ref >> "."
      case RepeatStep(cond, body) =>
        app >> First("repeat, ")
        for { c <- cond } app >> "while " >> c >> ","
        app >> body
      case PushCtxtStep(ref) =>
        app >> First("push ") >> ref >> " onto the execution context stack;"
        app >> " " >> ref >> " is now the running execution context."
      case NoteStep(note) =>
        app >> "NOTE: " >> note
      case SuspendStep(context, remove) =>
        app >> First("suspend ") >> context
        if (remove) app >> " and remove it from the execution context stack"
        app >> "."
      case SetEvaluationStateStep(context, param, step) =>
        app >> First("set the code evaluation state of ") >> context >> " "
        app >> "such that when evaluation is resumed "
        param match
          case None    => app >> "for that execution context "
          case Some(p) => app >> "with a " >> p >> " "
        app >> "the following steps will be performed:" >> step
      case ResumeEvaluationStep(context, argOpt, paramOpt, body) =>
        given Rule[Step] = stepWithUpperRule(true)
        app >> """<emu-meta effects="user-code">"""
        app >> "Resume the suspended evaluation of " >> context
        app >> "</emu-meta>"
        for (arg <- argOpt)
          app >> " using " >> arg
          app >> " as the result of the operation that suspended it"
        app >> "."
        for (param <- paramOpt)
          app >> " Let " >> param
          app >> " be the value returned by the resumed computation."
        for (step <- body)
          app :> "1. " >> step // TODO
        app
      case ReturnToResumeStep(context, retStep) =>
        given Rule[Step] = stepWithUpperRule(true)
        app >> retStep
        app :> "1. NOTE: This returns to the evaluation of the operation "
        app >> "that had most previously resumed evaluation of "
        app >> context >> "."
      case BlockStep(block) =>
        app >> block
      case YetStep(expr) =>
        app >> expr
    }
  }
  private case class First(str: String)
  private def firstRule(upper: Boolean): Rule[First] = (app, first) => {
    val First(str) = first
    app >> (if (upper) str.head.toUpper else str.head) >> str.substring(1)
  }
  private def endWithExprRule: Rule[Expression] = (app, expr) => {
    expr match {
      case multi: MultilineExpression => app >> expr
      case _                          => app >> expr >> "."
    }
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
      case RecordExpression(ty, fields) =>
        given Rule[(FieldLiteral, Expression)] = {
          case (app, (field, expr)) =>
            app >> field >> ": " >> expr
        }
        given Rule[List[(FieldLiteral, Expression)]] =
          iterableRule("{ ", ", ", " }")
        app >> ty >> " "
        if (fields.isEmpty) app >> "{ }"
        else app >> fields
      case LengthExpression(expr) =>
        app >> "the length of " >> expr
      case SubstringExpression(expr, from, to) =>
        app >> "the substring of " >> expr >> " from " >> from
        to.fold(app)(app >> " to " >> _)
      case NumberOfExpression(expr) =>
        app >> "the number of elements in " >> expr
      case SourceTextExpression(expr) =>
        app >> "the source text matched by " >> expr
      case CoveredByExpression(code, rule) =>
        app >> "the " >> rule >> " that is covered by " >> code
      case GetItemsExpression(nt, expr) =>
        app >> "the List of " >> nt >> " items in " >> expr
        app >> ", in source text order"
      case IntrinsicExpression(intr) =>
        app >> intr
      case XRefExpression(kind, id) =>
        app >> kind >> " <emu-xref href=\"#" >> id >> "\"></emu-xref>"
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
      case ListExpression(Nil) => app >> "« »"
      case ListExpression(entries) =>
        given Rule[Iterable[Expression]] = iterableRule("« ", ", ", " »")
        app >> entries
      case SoleElementExpression(expr) =>
        app >> "the sole element of " >> expr
      case CodeUnitAtExpression(base, index) =>
        app >> "the code unit at index " >> index >> " within " >> base
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
    app >> "the "
    (op, args) match
      case (Neg, List(e)) =>
        app >> "negation of " >> e
      case (Add, List(l, r)) =>
        app >> "sum of " >> l >> " and " >> r
      case (Mul, List(l, r)) =>
        app >> "product of " >> l >> " and " >> r
      case (Sub, List(l, r)) =>
        app >> "difference " >> l >> " minus " >> r
      case (Pow, List(l, r)) =>
        app >> "raising " >> l >> " to the " >> r >> " power"
      case (Expm1, List(e)) =>
        app >> "subtracting 1 from the exponential function of " >> e
      case (Log10, List(e)) =>
        app >> "base 10 logarithm of " >> e
      case (Log2, List(e)) =>
        app >> "base 2 logarithm of " >> e
      case (Cos, List(e)) =>
        app >> "cosine of " >> e
      case (Cbrt, List(e)) =>
        app >> "cube root of " >> e
      case (Exp, List(e)) =>
        app >> "exponential function of " >> e
      case (Cosh, List(e)) =>
        app >> "hyperbolic cosine of " >> e
      case (Sinh, List(e)) =>
        app >> "hyperbolic sine of " >> e
      case (Tanh, List(e)) =>
        app >> "hyperbolic tangent of " >> e
      case (Acos, List(e)) =>
        app >> "inverse cosine of " >> e
      case (Acosh, List(e)) =>
        app >> "inverse hyperbolic cosine of " >> e
      case (Asinh, List(e)) =>
        app >> "inverse hyperbolic sine of " >> e
      case (Atanh, List(e)) =>
        app >> "inverse hyperbolic tangent of " >> e
      case (Asin, List(e)) =>
        app >> "inverse sine of " >> e
      case (Atan2, List(x, y)) =>
        app >> "inverse tangent of the quotient " >> x >> " / " >> y
      case (Atan, List(e)) =>
        app >> "inverse tangent of " >> e
      case (Log1p, List(e)) =>
        app >> "natural logarithm of 1 + " >> e
      case (Log, List(e)) =>
        app >> "natural logarithm of " >> e
      case (Sin, List(e)) =>
        app >> "sine of " >> e
      case (Sqrt, List(e)) =>
        app >> "square root of " >> e
      case (Tan, List(e)) =>
        app >> "tangent of " >> e
      case (Hypot, List(l)) =>
        app >> "square root of the sum of squares of "
        app >> "the mathematical values of the elements of " >> l
      case _ => error(s"invalid math operationr: $op with $args")

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
        app >> "that captures " >> captured
        app >> " and performs the following steps when called:" >> body
    }

  // calculation expressions
  given calcExprRule: Rule[CalcExpression] = calcExprRuleWithLevel(0)

  def calcExprRuleWithLevel(level: Int): Rule[CalcExpression] = (app, expr) =>
    import ConversionExpressionOperator.*
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
      case ConversionExpression(ToApproxNumber, expr) =>
        app >> "an implementation-approximated Number value representing "
        app >> expr
      case ConversionExpression(o, e: (CalcExpression | InvokeExpression)) =>
        given Rule[ConversionExpressionOperator] = convExprOpRule(text = false)
        app >> o >> "(" >> e >> ")"
      case ConversionExpression(op, expr) =>
        given Rule[ConversionExpressionOperator] = convExprOpRule(text = true)
        app >> "the " >> op >> " value of " >> expr
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
      case Max   => "max"
      case Min   => "min"
      case Abs   => "abs"
      case Floor => "floor"
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
      case Algo          => "the definition specified in"
      case InternalSlots => "the internal slots listed in"
      case ParamLength =>
        "the number of non-optional parameters of the function definition in"
    })

  // literals
  given litRule: Rule[Literal] = (app, lit) =>
    lit match {
      case _: ThisLiteral      => app >> "*this* value"
      case _: NewTargetLiteral => app >> "NewTarget"
      case HexLiteral(hex, name) =>
        app >> f"0x$hex%04x"
        name.map(app >> " (" >> _ >> ")")
        app
      case CodeLiteral(code) => app >> "`" >> code >> "`"
      case NonterminalLiteral(ordinal, name, flags) =>
        given Rule[Iterable[String]] = iterableRule("[", ", ", "]")
        ordinal.map(ordinal => app >> "the " >> ordinal.toOrdinal >> " ")
        app >> "|" >> name
        if (!flags.isEmpty) app >> flags
        app >> "|"
      case ConstLiteral(name) => app >> "~" >> name >> "~"
      case StringLiteral(str) =>
        val replaced = str
          .replace("\\", "\\\\")
          .replace("*", "\\*")
        app >> "*\"" >> replaced >> "\"*"
      case FieldLiteral(name) => app >> "[[" >> name >> "]]"
      case SymbolLiteral(sym) => app >> "@@" >> sym
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
      case _: AbsentLiteral        => app >> "absent"
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
      case InvokeAbstractOperationExpression(name, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> name >> args
      case InvokeNumericMethodExpression(base, name, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> base >> "::" >> name >> args
      case InvokeAbstractClosureExpression(x, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> x >> args
      case InvokeMethodExpression(base, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> base >> args
      case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        // handle Evaluation
        if (name == "Evaluation") app >> "the result of evaluating " >> base
        // XXX handle Conatins, Contains always takes one argument
        else if (name == "Contains") app >> base >> " Contains " >> args.head
        // Otherwise
        else {
          app >> name >> " of " >> base
          if (!args.isEmpty) {
            app >> " with argument" >> (if (args.length > 1) "s " else " ")
            app >> args
          }
        }
        app
    }

  // conditions
  given condRule: Rule[Condition] = withLoc { (app, cond) =>
    cond match {
      case ExpressionCondition(expr) =>
        app >> expr
      case InstanceOfCondition(expr, neg, ty) =>
        app >> expr
        // TODO use a/an based on the types
        given Rule[Type] = (app, ty) => typeRule(app >> "a ", ty)
        ty match {
          case t :: Nil => app >> isStr(neg) >> t
          case _ =>
            app >> " is "
            val (left, namedSep) =
              if (neg) ("neither ", "nor") else ("either ", "or")
            given Rule[List[Type]] = listNamedSepRule(left, namedSep)
            app >> ty
        }
      case HasFieldCondition(ref, neg, field) =>
        app >> ref >> hasStr(neg)
        // TODO use a/an based on the fields
        app >> "a"
        app >> " " >> field >> " internal slot"
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
      case InclusiveIntervalCondition(left, neg, from, to) =>
        app >> left >> " is"
        if (neg) app >> " not"
        app >> " in the inclusive interval from " >> from
        app >> " to " >> to
      case ContainsWhoseCondition(list, ty, fieldName, expr) =>
        app >> list >> " contains a " >> ty
        app >> " whose [[" >> fieldName >> "]] is " >> expr
      case CompoundCondition(left, op, right) =>
        op match {
          case CompoundConditionOperator.Imply =>
            // TODO handle upper case of `if`
            app >> "If " >> left >> ", then " >> right
          case _ => app >> left >> " " >> op >> " " >> right
        }
    }
  }

  // operators for predicate conditions
  given predCondOpRule: Rule[PredicateConditionOperator] = (app, op) =>
    import PredicateConditionOperator.*
    app >> (op match {
      case Finite           => "finite"
      case Abrupt           => "an abrupt completion"
      case NeverAbrupt      => "never an abrupt completion"
      case Normal           => "a normal completion"
      case Duplicated       => "duplicate entries"
      case Present          => "present"
      case Empty            => "empty"
      case StrictMode       => "strict mode code"
      case ArrayIndex       => "an array index"
      case NonNegative      => "a non-negative integral Number"
      case FalseToken       => "the token `false`"
      case TrueToken        => "the token `true`"
      case DataProperty     => "a data property"
      case AccessorProperty => "an accessor property"
      case FullyPopulated   => "a fully populated Property Descriptor"
      case Nonterminal      => "an instance of a nonterminal"
      case IntegralNumber   => "an integral Number"
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
      case Contains         => "contains"
      case NContains        => "does not contain"
    })

  // operators for compound conditions
  given compCondOpRule: Rule[CompoundConditionOperator] = (app, op) => {
    import CompoundConditionOperator.*
    app >> (op match {
      case And   => "and"
      case Or    => "or"
      case Imply => "then" // XXX not used
    })
  }

  // references
  given refRule: Rule[Reference] = withLoc { (app, ref) =>
    ref match {
      case Variable(name: String) =>
        app >> s"_${name}_"
      case _: CurrentRealmRecord =>
        app >> "the current Realm Record"
      case _: ActiveFunctionObject =>
        app >> "the active function object"
      case _: RunningExecutionContext =>
        app >> "the running execution context"
      case _: SecondExecutionContext =>
        app >> "the second to top element of the execution context stack"
      case PropertyReference(base, nt: NonterminalProperty) =>
        app >> nt >> " " >> base
      case PropertyReference(base, prop) =>
        app >> base >> prop
    }
  }

  // properties
  given propRule: Rule[Property] = (app, prop) =>
    prop match {
      case FieldProperty(f)          => app >> ".[[" >> f >> "]]"
      case ComponentProperty(name)   => app >> "." >> name
      case IndexProperty(index)      => app >> "[" >> index >> "]"
      case IntrinsicProperty(intr)   => app >> ".[[" >> intr >> "]]"
      case NonterminalProperty(name) => app >> "the |" >> name >> "| of"
    }

  // intrinsics
  given intrRule: Rule[Intrinsic] = (app, intr) =>
    val Intrinsic(base, props) = intr
    app >> "%" >> base
    props.map(app >> "." >> _)
    app >> "%"

  // types
  given typeRule: Rule[Type] = (app, ty) =>
    ty.ty match
      case UnknownTy(msg) => app >> msg.getOrElse("unknown")
      case ty: ValueTy    => valueTyRule(false, false)(app, ty)

  // predefined types
  lazy val predTys: List[(PureValueTy, String)] = List(
    ESPureValueT -> "ECMAScript language value",
  )

  // value types
  def valueTyRule(
    plural: Boolean,
    withEither: Boolean,
  ): Rule[ValueTy] = (app, ty) =>
    val pure = !ty.pureValue.isBottom
    if (!ty.comp.isBottom)
      given Rule[PureValueTy] = pureValueTyRule(plural, true)
      val normal = !ty.normal.isBottom
      val abrupt = !ty.abrupt.isBottom
      val both = normal && abrupt
      if (both) app >> "either "
      val normalStr =
        if (normal)
          val app = new Appender
          app >> "a normal completion"
          if (!ty.normal.isTop) app >> " containing " >> ty.normal
          app.toString
        else ""
      app >> normalStr
      if (both) app >> (if (normalStr contains " or ") ", or " else " or ")
      if (abrupt) ty.abrupt match
        case Inf => app >> "an abrupt completion"
        case Fin(names) =>
          given Rule[List[String]] = listNamedSepRule(namedSep = "or")
          app >> names.toList.map("a " + _ + " completion")
      if (pure) app >> " or "
    if (pure)
      given Rule[PureValueTy] = pureValueTyRule(plural, withEither)
      app >> ty.pureValue
    app

  // pure value types
  def pureValueTyRule(
    plural: Boolean,
    withEither: Boolean,
  ): Rule[PureValueTy] = (app, originTy) =>
    var ty: PureValueTy = originTy
    var tys: Vector[String] = Vector()
    for ((pred, name) <- predTys if pred <= ty)
      tys :+= name.withArticle(plural); ty --= pred

    // names
    for (name <- ty.name.set) tys :+= name.withArticle(plural)

    // lists
    for (vty <- ty.list.elem)
      val sub = valueTyRule(true, true)(new Appender, vty)
      tys :+= s"a List of $sub"

    // symbols
    if (!ty.symbol.isBottom) tys :+= "a Symbol"

    // AST values
    ty.astValue match
      case AstTopTy => tys :+= "Parse Node".withArticle(plural)
      case ty: AstNonTopTy =>
        val set = ty.toName.names
        for (name <- set.toList.sorted)
          if (plural) tys :+= s"|$name| Parse Node${name.pluralPostfix}"
          else tys :+= s"${name.indefArticle} |$name| Parse Node"

    // consts
    for (name <- ty.const.toList.sorted) tys :+= s"~$name~"

    // numbers
    if (!ty.number.isBottom) tys :+= "Number".withArticle(plural)

    // big integers
    if (!ty.bigInt.isBottom) tys :+= "BigInt".withArticle(plural)

    // strings
    ty.str match
      case Inf      => tys :+= "String".withArticle(plural)
      case Fin(set) => for (s <- set.toList.sorted) tys :+= s"\"$s\""

    // booleans
    if (ty.bool.set.size > 1) tys :+= "Boolean".withArticle(plural)
    else if (ty.bool.set.size == 1) tys :+= s"*${ty.bool.set.head}*"

    // undefined
    if (!ty.undef.isBottom) tys :+= "*undefined*"

    // null
    if (!ty.nullv.isBottom) tys :+= "*null*"

    given Rule[Iterable[String]] = listNamedSepRule(namedSep = "or")
    if (withEither && tys.length >= 2) app >> "either "
    app >> tys

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
}
