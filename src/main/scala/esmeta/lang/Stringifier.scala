package esmeta.lang

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

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
          case Some(ifStep: IfStep) => app :> "1. Else " >> ifStep
          case Some(step)           => app :> "1. Else," >> step
          case None                 => app
        }
      case ReturnStep(expr) =>
        app >> First("return ") >> expr >> "."
      case AssertStep(cond) =>
        app >> First("assert: ") >> cond >> "."
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
      case TypeCheckExpression(expr, ty, neg) =>
        app >> "Type(" >> expr >> ") is "
        app >> (if (neg) "not " else "") >> ty
      case LengthExpression(expr) =>
        app >> "the length of " >> expr
      case SubstringExpression(expr, from, to) =>
        app >> "the substring of " >> expr >> " from " >> from >> " to " >> to
      case expr: CalcExpression =>
        app >> expr
      case expr: InvokeExpression => app >> expr
      case ReturnIfAbruptExpression(expr, check) =>
        app >> (if (check) "?" else "!") >> " " >> expr
      case ListExpression(Nil) => app >> "« »"
      case ListExpression(entries) =>
        given Rule[Iterable[Expression]] = iterableRule("« ", ", ", " »")
        app >> entries
      case NonterminalExpression(name) =>
        app >> "|" >> name >> "|"
      case YetExpression(str, block) =>
        app >> "[YET] " >> str
        block.fold(app)(app >> _)
    }

  // calculation expressions
  // TODO consider the appropriate parenthesis
  given calcExprRule: Rule[CalcExpression] = (app, expr) =>
    expr match {
      case ReferenceExpression(ref) =>
        app >> ref
      case BinaryExpression(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case UnaryExpression(op, expr) =>
        app >> op >> expr
      case lit: Literal => app >> lit
    }

  // algorithm invocation expressions
  given invokeExprRule: Rule[InvokeExpression] = (app, expr) =>
    expr match {
      case InvokeAbstractOperationExpression(name, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> name >> args
      case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        app >> name >> " of " >> base
        if (!args.isEmpty) {
          app >> " using " >> args >> " as the argument"
          if (args.length > 1) app >> "s"
        }
        app
    }

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
      case ThisLiteral                      => app >> "*this* value"
      case ConstLiteral(name)               => app >> "~" >> name >> "~"
      case StringLiteral(str)               => app >> "*\"" >> str >> "\"*"
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

  // conditions
  given condRule: Rule[Condition] = (app, cond) =>
    cond match {
      case ExpressionCondition(expr) =>
        app >> expr
      case HasFieldCondition(expr, fieldName) =>
        // TODO use a/an based on the field name
        app >> expr >> " has "
        app >> "a"
        app >> " [[" >> fieldName >> "]] internal slot"
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
  given refRule: Rule[Reference] = (app, id) =>
    id match {
      case Field(base, name) =>
        app >> base >> ".[[" >> name >> "]]"
      case Variable(name: String) =>
        app >> s"_${name}_"
    }

  // types
  given typeRule: Rule[Type] = (app, ty) => app >> ty.name

  // list with named separator
  def listNamedSepRule[T](
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
}
