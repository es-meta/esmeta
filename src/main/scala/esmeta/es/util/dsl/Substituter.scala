package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.Walker as LangWalker

import AstExtensions.*

object Substituter {

  private def mkWalker(bindings: CaptureEnv): LangWalker = new LangWalker {
    override def walk(step: Step): Step = step match {
      case MetaStep(name, ml, v) =>
        bindings
          .get(CaptureKey(name, v))
          .map(_.asInstanceOf[Step])
          .getOrElse(MetaStep(name, ml, v))
      case _ => super.walk(step)
    }

    override def walk(expr: Expression): Expression = expr match {
      case MetaExpression(name, v) =>
        bindings
          .get(CaptureKey(name, v))
          .map(_.asInstanceOf[Expression])
          .getOrElse(MetaExpression(name, v))
      case _ => super.walk(expr)
    }

    override def walk(cond: Condition): Condition = cond match {
      case MetaCondition(name, v) =>
        bindings
          .get(CaptureKey(name, v))
          .map(_.asInstanceOf[Condition])
          .getOrElse(MetaCondition(name, v))
      case _ => super.walk(cond)
    }

    override def walk(ref: Reference): Reference = ref match {
      case MetaReference(name, v) =>
        bindings
          .get(CaptureKey(name, v))
          .map(asRef)
          .getOrElse(MetaReference(name, v))
      case Variable(name, nt, true, variant) =>
        bindings
          .get(CaptureKey(name, variant))
          .map(asRef)
          .getOrElse(Variable(name, nt, true, variant))
      case _ => super.walk(ref)
    }

    private def asRef(elem: LangElem): Reference = elem match {
      case ref: Reference           => ref
      case ReferenceExpression(ref) => ref
      case other =>
        throw new RuntimeException(
          s"Cannot coerce ${other.getClass.getSimpleName} to Reference",
        )
    }

    override def walk(x: Variable): Variable = x match {
      case Variable(name, nt, true, variant) =>
        bindings
          .get(CaptureKey(name, variant))
          .map(_.asInstanceOf[Variable])
          .getOrElse(Variable(name, nt, true, variant))
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
  def substRule(
    rule: Rule[LangElem],
    bindings: CaptureEnv,
  ): Rule[LangElem] = rule match {
    case r: StepRule =>
      r.copy(
        pattern = subst(r.pattern, bindings),
        replace = r.replace.map(subst(_, bindings)),
        subrules = r.subrules.map(substRule(_, bindings)),
      )
    case r: ExpressionRule =>
      r.copy(
        pattern = subst(r.pattern, bindings),
        replace = r.replace.map(subst(_, bindings)),
      )
    case r: ConditionRule =>
      r.copy(
        pattern = subst(r.pattern, bindings),
        replace = r.replace.map(subst(_, bindings)),
      )
    case r: ReferenceRule =>
      r.copy(
        pattern = subst(r.pattern, bindings),
        replace = r.replace.map(subst(_, bindings)),
      )
  }
}
