package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.{Walker => LangWalker}

import AstExtensions.*

object Substituter {

  private def mkWalker(bindings: CaptureEnv): LangWalker = new LangWalker {
    override def walk(step: Step): Step = step match {
      case MetaStep(name, false) => bindings(name).asInstanceOf[Step]
      case MetaStep(name, true)  => bindings(name).asInstanceOf[Step]
      case _                     => super.walk(step)
    }

    override def walk(expr: Expression): Expression = expr match {
      case MetaExpression(name) =>
        bindings(name).asInstanceOf[Expression]
      case _ => super.walk(expr)
    }

    override def walk(cond: Condition): Condition = cond match {
      case MetaCondition(name) =>
        bindings(name).asInstanceOf[Condition]
      case _ => super.walk(cond)
    }

    override def walk(ref: Reference): Reference = ref match {
      case MetaReference(name) =>
        bindings(name).asInstanceOf[Reference]
      case Variable(name, _) if name.startsWith("$") =>
        bindings(name).asInstanceOf[Reference]
      case _ => super.walk(ref)
    }

    // Fix L19: Variable in AbstractClosureExpression.params is walked
    // via walk(Variable), not walk(Reference). Override to handle
    // dollar-prefixed substitution.
    override def walk(x: Variable): Variable = x match {
      case Variable(name, _) if name.startsWith("$") =>
        bindings(name).asInstanceOf[Variable]
      case _ => super.walk(x)
    }
  }

  def subst(step: Step, bindings: CaptureEnv): Step =
    mkWalker(bindings).walk(step)

  def subst(expr: Expression, bindings: CaptureEnv): Expression =
    mkWalker(bindings).walk(expr)

  def subst(cond: Condition, bindings: CaptureEnv): Condition =
    mkWalker(bindings).walk(cond)

  def subst(ref: Reference, bindings: CaptureEnv): Reference =
    mkWalker(bindings).walk(ref)
}
