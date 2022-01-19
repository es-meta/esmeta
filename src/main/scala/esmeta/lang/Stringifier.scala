package esmeta.lang

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.lang.Utils.*

/** stringifier for language */
case class Stringifier(detail: Boolean) {
  // elements
  given elemRule: Rule[LangElem] = (app, elem) =>
    elem match {
      case elem: Program              => programRule(app, elem)
      case elem: Block                => blockRule(app, elem)
      case elem: Step                 => stepRule(app, elem)
      case elem: SubStep              => subStepRule(app, elem)
      case elem: Expression           => exprRule(app, elem)
      case elem: Condition            => condRule(app, elem)
      case elem: Reference            => refRule(app, elem)
      case elem: Type                 => typeRule(app, elem)
      case elem: Property             => propRule(app, elem)
      case elem: Field                => fieldRule(app, elem)
      case elem: Intrinsic            => intrRule(app, elem)
      case elem: MathOpExpression.Op  => mathOpExprOpRule(app, elem)
      case elem: BinaryExpression.Op  => binExprOpRule(app, elem)
      case elem: UnaryExpression.Op   => unExprOpRule(app, elem)
      case elem: BinaryCondition.Op   => binCondOpRule(app, elem)
      case elem: CompoundCondition.Op => compCondOpRule(app, elem)
    }

  // programs
  given programRule: Rule[Program] = _ >> _.block

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
    val SubStep(idTag, step) = subStep
    idTag.map(app >> "[id=\"" >> _ >> "\"] ")
    app >> step

  // steps
  given stepRule: Rule[Step] = stepWithUpperRule(false)
  def stepWithUpperRule(upper: Boolean): Rule[Step] = (app, step) =>
    given Rule[First] = firstRule(upper)
    step match {
      case LetStep(x, expr) =>
        app >> First("let ") >> x >> " be " >> expr >> "."
      case SetStep(x, expr) =>
        app >> First("set ") >> x >> " to " >> expr >> "."
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
        app >> First("return")
        expr.map(app >> " " >> _)
        app >> "."
      case AssertStep(cond) =>
        app >> First("assert: ") >> cond >> "."
      case ForEachStep(ty, elem, expr, body) =>
        app >> First("for each ")
        ty.map(app >> _ >> " ")
        app >> elem >> " of " >> expr >> ", "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachIntegerStep(x, start, cond, ascending, body) =>
        app >> First("for each integer ") >> x >> " starting with " >> start
        app >> " such that " >> cond >> ", in "
        app >> (if (ascending) "ascending" else "descending") >> " order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ThrowStep(errorName) =>
        app >> First("throw a *") >> errorName >> "* exception."
      case PerformStep(expr) =>
        app >> First("perform ") >> expr >> "."
      case AppendStep(expr, ref) =>
        app >> First("append ") >> expr >> " to " >> ref >> "."
      case RepeatStep(cond, body) =>
        app >> First("repeat, ")
        for { c <- cond } app >> "while " >> c >> ","
        app >> body
      case PushStep(context) =>
        app >> First("push ") >> context >> " onto the execution context stack;"
        app >> " " >> context >> " is now the running execution context."
      case NoteStep(note) =>
        app >> "NOTE: " >> note
      case SuspendStep(context) =>
        app >> First("suspend ") >> context >> "."
      case BlockStep(block) =>
        app >> block
      case YetStep(expr) =>
        app >> expr
    }
  private case class First(str: String)
  private def firstRule(upper: Boolean): Rule[First] = (app, first) => {
    val First(str) = first
    app >> (if (upper) str.head.toUpper else str.head) >> str.substring(1)
  }

  // expressions
  given exprRule: Rule[Expression] = (app, expr) =>
    expr match {
      case StringConcatExpression(exprs) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        app >> "the string-concatenation of " >> exprs
      case ListConcatExpression(exprs) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        app >> "the list-concatenation of " >> exprs
      case RecordExpression(ty, fields) =>
        given Rule[(Field, Expression)] = { case (app, (field, expr)) =>
          app >> field >> ": " >> expr
        }
        given Rule[List[(Field, Expression)]] = iterableRule("{ ", ", ", " }")
        app >> ty >> " "
        if (fields.isEmpty) app >> "{ }"
        else app >> fields
      case TypeCheckExpression(expr, neg, ty) =>
        app >> "Type(" >> expr >> ")" >> isStr(neg) >> ty
      case LengthExpression(expr) =>
        app >> "the length of " >> expr
      case SubstringExpression(expr, from, to) =>
        app >> "the substring of " >> expr >> " from " >> from >> " to " >> to
      case IntrinsicExpression(intr) =>
        app >> intr
      case expr: CalcExpression =>
        calcExprRule(app, expr)
      case expr: InvokeExpression =>
        invokeExprRule(app, expr)
      case ReturnIfAbruptExpression(expr, check) =>
        app >> (if (check) "?" else "!") >> " " >> expr
      case ListExpression(Nil) => app >> "« »"
      case ListExpression(entries) =>
        given Rule[Iterable[Expression]] = iterableRule("« ", ", ", " »")
        app >> entries
      case YetExpression(str, block) =>
        app >> "[YET] " >> str
        block.fold(app)(app >> _)
    }

  // calculation expressions
  given calcExprRule: Rule[CalcExpression] = calcExprRuleWithLevel(0)

  def calcExprRuleWithLevel(level: Int): Rule[CalcExpression] = (app, expr) =>
    given Rule[CalcExpression] = calcExprRuleWithLevel(expr.level)
    if (expr.level < level) app >> "("
    expr match {
      case ReferenceExpression(ref) =>
        app >> ref
      case MathOpExpression(op, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> op >> args
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

  // operators for mathematical operation expressions
  given mathOpExprOpRule: Rule[MathOpExpression.Op] = (app, op) =>
    import MathOpExpression.Op.*
    app >> (op match {
      case Max      => "max"
      case Min      => "min"
      case Abs      => "abs"
      case Floor    => "floor"
      case ToBigInt => "ℤ"
      case ToNumber => "𝔽"
      case ToMath   => "ℝ"
    })

  // operators for binary expressions
  given binExprOpRule: Rule[BinaryExpression.Op] = (app, op) =>
    import BinaryExpression.Op.*
    app >> (op match {
      case Add => "+"
      case Sub => "-"
      case Mul => "×"
      case Div => "/"
      case Mod => "modulo"
    })

  // operators for unary expressions
  given unExprOpRule: Rule[UnaryExpression.Op] = (app, op) =>
    import UnaryExpression.Op.*
    app >> (op match {
      case Neg => "-"
    })

  // literals
  given litRule: Rule[Literal] = (app, lit) =>
    lit match {
      case ThisLiteral      => app >> "*this* value"
      case NewTargetLiteral => app >> "NewTarget"
      case HexLiteral(hex, name) =>
        app >> f"0x$hex%04x"
        name.map(app >> " (" >> _ >> ")")
        app
      case CodeLiteral(code) => app >> "`" >> code >> "`"
      case NonterminalLiteral(ordinal, name) =>
        ordinal.map(ordinal => app >> "the " >> ordinal.toOrdinal >> " ")
        app >> "|" >> name >> "|"
      case ConstLiteral(name) => app >> "~" >> name >> "~"
      case StringLiteral(str) =>
        val replaced = str
          .replace("\\", "\\\\")
          .replace("*", "\\*")
        app >> "*\"" >> replaced >> "\"*"
      case FieldLiteral(field)              => app >> field
      case SymbolLiteral(sym)               => app >> "@@" >> sym
      case PositiveInfinityMathValueLiteral => app >> "+∞"
      case NegativeInfinityMathValueLiteral => app >> "-∞"
      case DecimalMathValueLiteral(n)       => app >> n
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
      case BigIntLiteral(n) => app >> "*" >> n >> "*<sub>ℤ</sub>"
      case TrueLiteral      => app >> "*true*"
      case FalseLiteral     => app >> "*false*"
      case UndefinedLiteral => app >> "*undefined*"
      case NullLiteral      => app >> "*null*"
    }

  // algorithm invocation expressions
  given invokeExprRule: Rule[InvokeExpression] = (app, expr) =>
    expr match {
      case InvokeAbstractOperationExpression(name, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> name >> args
      case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        // handle Evaluation
        if (name == "Evaluation") app >> "the result of evaluating " >> base
        else app >> name >> " of " >> base
        if (!args.isEmpty) {
          app >> " using " >> args >> " as the argument"
          if (args.length > 1) app >> "s"
        }
        app
    }

  // conditions
  given condRule: Rule[Condition] = (app, cond) =>
    cond match {
      case ExpressionCondition(expr) =>
        app >> expr
      case InstanceOfCondition(expr, neg, ty) =>
        app >> expr >> isStr(neg)
        // TODO use a/an based on the types
        app >> "a"
        app >> " " >> ty
      case HasFieldCondition(expr, neg, field) =>
        app >> expr >> hasStr(neg)
        // TODO use a/an based on the fields
        app >> "a"
        app >> " " >> field >> " internal slot"
      case AbruptCompletionCondition(x, neg) =>
        app >> x >> isStr(neg) >> "an abrupt completion"
      case BinaryCondition(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case CompoundCondition(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
    }

  // operators for binary conditions
  given binCondOpRule: Rule[BinaryCondition.Op] = (app, op) =>
    import BinaryCondition.Op.*
    app >> (op match {
      case Is               => "is"
      case NIs              => "is not"
      case Eq               => "="
      case NEq              => "≠"
      case LessThan         => "<"
      case LessThanEqual    => "≤"
      case GreaterThan      => ">"
      case GreaterThanEqual => "≥"
      case SameCodeUnits    => "is the same sequence of code units as"
    })

  // operators for compound conditions
  given compCondOpRule: Rule[CompoundCondition.Op] = (app, op) => {
    import CompoundCondition.Op.*
    app >> (op match {
      case And => "and"
      case Or  => "or"
    })
  }

  // references
  given refRule: Rule[Reference] = (app, ref) =>
    ref match {
      case Variable(name: String) =>
        app >> s"_${name}_"
      case CurrentRealmRecord =>
        app >> "the current Realm Record"
      case RunningExecutionContext =>
        app >> "the running execution context"
      case PropertyReference(base, prop) =>
        app >> base >> prop
    }

  // properties
  given propRule: Rule[Property] = (app, prop) =>
    prop match {
      case FieldProperty(f)        => app >> "." >> f
      case ComponentProperty(name) => app >> "." >> name
      case IndexProperty(index)    => app >> "[" >> index >> "]"
    }

  // fields
  given fieldRule: Rule[Field] = (app, field) =>
    app >> "[["
    field match {
      case StringField(name)         => app >> name
      case IntrinsicField(intrinsic) => app >> intrinsic
    }
    app >> "]]"

  // intrinsics
  given intrRule: Rule[Intrinsic] = (app, intr) =>
    val Intrinsic(base, props) = intr
    app >> "%" >> base
    props.map(app >> "." >> _)
    app >> "%"

  // types
  given typeRule: Rule[Type] = (app, ty) => app >> ty.name

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // list with named separator
  private def listNamedSepRule[T](
    left: String = "",
    namedSep: String = "",
    right: String = "",
  )(using tRule: Rule[T]): Rule[List[T]] = (app, list) =>
    val len = list.length
    app >> left
    for ((x, k) <- list.zipWithIndex) app >> (k match {
      case 0                 => ""
      case _ if k == len - 1 => (if (len > 2) ", " else " ") + namedSep + " "
      case _                 => ", "
    }) >> x
    app >> right

  // verbs
  private def isStr(neg: Boolean): String =
    if (neg) " is not " else " is "
  private def hasStr(neg: Boolean): String =
    if (neg) " does not have " else " has "
}
