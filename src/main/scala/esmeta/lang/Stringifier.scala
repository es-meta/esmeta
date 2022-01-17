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
      case elem: Expression           => exprRule(app, elem)
      case elem: Condition            => condRule(app, elem)
      case elem: Identifier           => idRule(app, elem)
      case elem: BinaryExpression.Op  => binExprOpRule(app, elem)
      case elem: BinaryCondition.Op   => binCondOpRule(app, elem)
      case elem: CompoundCondition.Op => compCondOpRule(app, elem)
    }

  // programs
  given programRule: Rule[Program] = _ >> _.block

  // blocks
  given blockRule: Rule[Block] = (app, block) =>
    if (detail) app.wrap("", "")(block match {
      case Order(steps) =>
        steps.foreach(app :> "1. " >> _)
      case Unorder(steps) =>
        steps.foreach(app :> "* " >> _)
      case Figure(lines) =>
        app :> "<figure>"
        app.wrap("", "")(lines.map(app :> _))
        app >> "</figure>"
    })
    else app >> " [...]"

  // TODO handle needUppercase
  // steps
  given stepRule: Rule[Step] = (app, step) =>
    step match {
      case LetStep(x, expr) =>
        app >> "let " >> x >> " be " >> expr >> "."
      case IfStep(cond, thenStep, elseStep) =>
        app >> "if " >> cond >> ", " >> thenStep
      case ReturnStep(expr) =>
        app >> "return " >> expr >> "."
      case AssertStep(cond) =>
        app >> "assert: " >> cond >> "."
      case ForEachIntegerStep(x, start, cond, ascending, body) =>
        app >> "for each integer " >> x >> " starting with " >> start
        app >> " such that " >> cond >> ", in "
        app >> (if (ascending) "ascending" else "descending") >> " order, do"
        app >> body
      case BlockStep(block) => app >> block
      case YetStep(str, block) =>
        app >> "[YET] " >> str
        block.fold(app)(app >> _)
    }

  // expressions
  given exprRule: Rule[Expression] = (app, expr) =>
    expr match {
      case LengthExpression(expr) =>
        app >> "the length of " >> expr
      case SubstringExpression(expr, from, to) =>
        app >> "the substring of " >> expr >> " from " >> from >> " to " >> to
      case EmptyStringExpression =>
        app >> "the empty String"
      case expr: CalcExpression =>
        app >> expr
    }

  // calculation expressions
  given calcExprRule: Rule[CalcExpression] = (app, expr) =>
    expr match {
      case IdentifierExpression(id) => app >> id
      case BinaryExpression(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case lit: Literal => app >> lit
    }

  // literals
  given litRule: Rule[Literal] = (app, lit) =>
    lit match {
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
            else if (n == 0) {
              if ((1 / n).isPosInfinity) "+0" else "-0"
            } else n.toString
          ) >> "*<sub>ℤ</sub>"
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
      case BinaryCondition(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case CompoundCondition(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
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
    })

  // operators for compound conditions
  given compCondOpRule: Rule[CompoundCondition.Op] = (app, op) => {
    import CompoundCondition.Op.*
    app >> (op match {
      case And => "and"
      case Or  => "or"
    })
  }

  // identifiers
  given idRule: Rule[Identifier] = (app, id) =>
    id match {
      case Variable(name: String) => app >> s"_${name}_"
    }
}
