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
      case elem: Program    => programRule(app, elem)
      case elem: Block      => blockRule(app, elem)
      case elem: Step       => stepRule(app, elem)
      case elem: Expression => exprRule(app, elem)
      case elem: Condition  => condRule(app, elem)
      case elem: Identifier => idRule(app, elem)
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
      case YetStep(str, block) =>
        app >> "[YET] " >> str
        block.fold(app)(app >> _)
    }

  // expressions
  given exprRule: Rule[Expression] = (app, expr) =>
    expr match {
      case LengthExpression(expr)   => app >> "the length of " >> expr
      case IdentifierExpression(id) => app >> id
      case lit: Literal             => app >> lit
    }

  // literals
  given litRule: Rule[Literal] = (app, lit) =>
    lit match {
      case EmptyStringLiteral               => app >> "the empty String"
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

  // binary operators
  given binOpRule: Rule[BinaryOp] = (app, op) =>
    import BinaryOp.*
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

  // compound operators
  given compRule: Rule[CompoundOp] = (app, op) => {
    import CompoundOp.*
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
