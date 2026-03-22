package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.{Walker => LangWalker}

import AstExtensions.*

object Substituter {

  private def mkWalker(bindings: CaptureEnv): LangWalker = new LangWalker {
    override def walk(step: Step): Step = step match {
      case MetaStep(name, ml) =>
        bindings
          .get(name)
          .map(_.asInstanceOf[Step])
          .getOrElse(MetaStep(name, ml))
      case _ => super.walk(step)
    }

    override def walk(expr: Expression): Expression = expr match {
      case MetaExpression(name) =>
        bindings
          .get(name)
          .map(_.asInstanceOf[Expression])
          .getOrElse(MetaExpression(name))
      case _ => super.walk(expr)
    }

    override def walk(cond: Condition): Condition = cond match {
      case MetaCondition(name) =>
        bindings
          .get(name)
          .map(_.asInstanceOf[Condition])
          .getOrElse(MetaCondition(name))
      case _ => super.walk(cond)
    }

    override def walk(ref: Reference): Reference = ref match {
      case MetaReference(name) =>
        bindings
          .get(name)
          .map(_.asInstanceOf[Reference])
          .getOrElse(MetaReference(name))
      case Variable(name, nt) if name.startsWith("$") =>
        bindings
          .get(name)
          .map(_.asInstanceOf[Reference])
          .getOrElse(Variable(name, nt))
      case _ => super.walk(ref)
    }

    override def walk(x: Variable): Variable = x match {
      case Variable(name, nt) if name.startsWith("$") =>
        bindings
          .get(name)
          .map(_.asInstanceOf[Variable])
          .getOrElse(Variable(name, nt))
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

  /** Substitute bindings into a Rule's patterns and templates. */
  def substRule(rule: Rule, bindings: CaptureEnv): Rule = rule match {
    case r: StepRule =>
      r.copy(
        pattern = subst(r.pattern, bindings),
        replace = r.replace.map(subst(_, bindings)),
        subrules = r.subrules.map(substRule(_, bindings)),
      )
    case r: ExpressionRule =>
      r.copy(
        pattern = subst(r.pattern, bindings),
        replace = subst(r.replace, bindings),
      )
    case r: ConditionRule =>
      r.copy(
        pattern = subst(r.pattern, bindings),
        replace = subst(r.replace, bindings),
      )
    case r: ReferenceRule =>
      r.copy(
        pattern = subst(r.pattern, bindings),
        replace = subst(r.replace, bindings),
      )
    case r: StepBlockRule =>
      r.copy(
        patternSteps = r.patternSteps.map(subst(_, bindings)),
        replace = r.replace.map(subst(_, bindings)),
        subrules = r.subrules.map(substRule(_, bindings)),
      )
    case r: WhereRule =>
      r.copy(
        wherePattern = subst(r.wherePattern, bindings),
        mainRules = r.mainRules.map(substRule(_, bindings)),
      )
  }
}
