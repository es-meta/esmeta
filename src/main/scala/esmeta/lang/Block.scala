package esmeta.lang

import esmeta.lang.util.*

// metalanguage blocks
sealed trait Block extends Syntax
object Block extends Parser.From(Parser.block)

case class StepBlock(steps: List[SubStep]) extends Block
case class ExprBlock(exprs: List[Expression]) extends Block
case class Figure(lines: List[String]) extends Block

// sub-steps with optional directives (e.g., id, legacy, normative-optional, ...)
case class SubStep(directives: List[Directive], step: Step) extends Syntax

// user-defined directives
case class Directive(name: String, values: List[String]) extends Syntax
