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
        app >> "jet " >> x >> " be " >> expr >> "."
      case IfStep(cond, thenStep, elseStep) =>
        app >> "if " >> cond >> ", " >> thenStep
      case ReturnStep(expr) =>
        app >> "return " >> expr >> "."
      case YetStep(str, block) =>
        app >> str
        block.fold(app)(app >> _)
    }

  // expressions
  given exprRule: Rule[Expression] = (app, expr) =>
    expr match {
      case LengthExpression(expr)   => app >> "the length of " >> expr
      case IdentifierExpression(id) => app >> id
      case EmptyString              => app >> "the empty String"
    }

  // conditions
  given condRule: Rule[Condition] = (app, cond) =>
    cond match {
      case ExpressionCondition(expr) =>
        app >> expr
      case EqualCondition(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case LogicalAndCondition(left, right) =>
        app >> left >> " and " >> right
    }

  // equal operators
  given eqOpRule: Rule[EqualOp] = (app, op) =>
    import EqualOp.*
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

  // identifiers
  given idRule: Rule[Identifier] = (app, id) =>
    id match {
      case Variable(name: String) => app >> s"_${name}_"
    }
}
