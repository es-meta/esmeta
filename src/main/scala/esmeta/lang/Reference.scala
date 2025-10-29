package esmeta.lang

import esmeta.lang.util.*

// metalanguage references
sealed trait Reference extends Syntax
object Reference extends Parser.From(Parser.ref)

// variables
case class Variable(name: String, nt: Option[String] = None) extends Reference

// access
case class Access(
  base: Reference,
  name: String,
  kind: AccessKind = AccessKind.Field,
  form: AccessForm = AccessForm.Dot,
) extends Reference

// Field: [[name]]
// Component: name (component)
enum AccessKind:
  case Field
  case Component(hasPostfix: Boolean = false) // ~ component

// Dot: base.field
// Of: the field of base
// Apo: base's field (desc)
enum AccessForm:
  case Dot, Of
  case Apo(desc: Option[String] = None)

// value of
case class ValueOf(base: Reference) extends Reference

// intrinsic field
case class IntrinsicField(
  base: Reference,
  intr: Intrinsic,
) extends Reference

// index lookup
case class IndexLookup(
  base: Reference,
  index: Expression,
) extends Reference

// binding lookup
case class BindingLookup(
  base: Reference,
  binding: Expression,
) extends Reference

// nonterminal lookup
case class NonterminalLookup(
  base: Reference,
  nt: String,
) extends Reference

// positional element
case class PositionalElement(
  base: Reference,
  isFirst: Boolean,
) extends Reference

// intrinsic objects with names
case class IntrinsicObject(
  base: Reference,
  expr: Expression,
) extends Reference

// the running execution context literals
case class RunningExecutionContext() extends Reference

// the second execution context
case class SecondExecutionContext() extends Reference

// the current realm record
case class CurrentRealmRecord() extends Reference

// the active function object
case class ActiveFunctionObject() extends Reference

// references to agent record
case class AgentRecord() extends Reference

// -----------------------------------------------------------------------------
// intrinsics
// -----------------------------------------------------------------------------
case class Intrinsic(base: String, props: List[String]) extends Syntax
object Intrinsic extends Parser.From(Parser.intr)
