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
case class PropertyReference(
  base: Reference,
  prop: Property,
) extends Reference

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

// Dot: base.[[ field ]]
// Value: {{ base }}'s [[ {{ field }} ]] value
// Attribute: the value of {{ base }}'s {{ field }} attribute
enum FieldPropertyForm:
  case Dot, Value, Attribute

// component property
case class ComponentProperty(name: String, form: ComponentPropertyForm)
  extends Property

// Dot: Something.Property
// Apostrophe: Something's Property
// Text: the Property of Something
enum ComponentPropertyForm:
  case Dot, Apostrophe
  case Text(desc: Option[String])

// binding property
case class BindingProperty(binding: Expression) extends Property

// index property
case class IndexProperty(index: Expression) extends Property

// positional element property (first, last)
case class PositionalElementProperty(isFirst: Boolean) extends Property

// intrinsic property
case class IntrinsicProperty(intrinsic: Intrinsic) extends Property

// nonterminal property
case class NonterminalProperty(name: String) extends Property

// -----------------------------------------------------------------------------
// intrinsics
// -----------------------------------------------------------------------------
case class Intrinsic(base: String, props: List[String]) extends Syntax
object Intrinsic extends Parser.From(Parser.intr)
