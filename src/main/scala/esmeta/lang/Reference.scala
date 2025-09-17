package esmeta.lang

import esmeta.lang.util.*

// metalanguage references
sealed trait Reference extends Syntax
object Reference extends Parser.From(Parser.ref)

// variables
case class Variable(name: String) extends Reference

// the running execution context literals
case class RunningExecutionContext() extends Reference

// the second execution context
case class SecondExecutionContext() extends Reference

// the current realm record
case class CurrentRealmRecord() extends Reference

// the active function object
case class ActiveFunctionObject() extends Reference

// references to property
case class PropertyReference(base: Reference, prop: Property) extends Reference

// references to agent record
case class AgentRecord() extends Reference

// -----------------------------------------------------------------------------
// metalanguage properties
// -----------------------------------------------------------------------------
sealed trait Property extends Syntax
object Property extends Parser.From(Parser.prop)

// field property
case class FieldProperty(
  name: String,
  form: FieldPropertyForm = FieldPropertyForm.Dot,
) extends Property

// Dot: "." ~> "[[" ~> word <~ "]]" ^^ { FieldProperty(_) }
// Attribute: ("the value of" ~> variable <~ "'s") ~ ("[[" ~> word <~ "]]" ~ "attribute")
// Value: (variable <~ "'s") ~ ("[[" ~> word <~ "]]") <~ "value"
// StrictBinding: ref ~ ("is" ^^^ false | "is not" ^^^ true) <~ "a strict binding"
// IntrinsicObject: (variable <~ "'s intrinsic object named") ~ variable
// InitCond: ref ~ ("has been" ^^^ false | "has not" ~ opt("yet") ~ "been" ^^^ true) <~ "initialized"
enum FieldPropertyForm {
  case Dot, Attribute, Value, StrictBinding, IntrinsicObject, InitCond
}

// component property
case class ComponentProperty(name: String, form: ComponentPropertyForm)
  extends Property

// Dot: Something.Property
// Apostrophe: Something's Property
// Text: the Property of Something
enum ComponentPropertyForm {
  case Dot, Apostrophe, Text
}

// binding property
case class BindingProperty(binding: Expression) extends Property

// index property
case class IndexProperty(index: Expression, isTextForm: Boolean = false)
  extends Property

// intrinsic property
case class IntrinsicProperty(intrinsic: Intrinsic) extends Property

// nonterminal property
case class NonterminalProperty(name: String) extends Property

// -----------------------------------------------------------------------------
// intrinsics
// -----------------------------------------------------------------------------
case class Intrinsic(base: String, props: List[String]) extends Syntax
object Intrinsic extends Parser.From(Parser.intr)
