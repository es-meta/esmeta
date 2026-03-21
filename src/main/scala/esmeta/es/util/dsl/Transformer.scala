package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.{Walker => LangWalker}

import scala.collection.mutable

import AstExtensions.*

/** Tracks rule match statistics. */
class TransformStats {
  private val counts = mutable.Map[String, Int]().withDefaultValue(0)

  def record(ruleName: String): Unit = counts(ruleName) += 1

  def printSummary(): Unit = {
    println()
    println("=== DSL Transformation Summary ===")
    counts.toList.sortBy(_._1).foreach {
      case (name, count) =>
        println(f"  $name%-50s : $count%3d")
    }
    val total = counts.values.sum
    println("-" * 60)
    println(f"  ${"Total"}%-50s : $total%3d")
    println()
  }
}

object Transformer {

  /** Apply a single rule to an entire AST tree rooted at a Step. */
  def transformStep(
    rule: Rule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats] = None,
  ): Step =
    rule match {
      case sr: StepRule       => applyStepRule(sr, step, ctx, stats)
      case er: ExpressionRule => applyExpressionRule(er, step, ctx, stats)
      case cr: ConditionRule  => applyConditionRule(cr, step, ctx, stats)
      case rr: ReferenceRule  => applyReferenceRule(rr, step, ctx, stats)
      case br: StepBlockRule  => applyStepBlockRule(br, step, ctx, stats)
    }

  private def onMatch[T](
    ruleName: String,
    before: T,
    after: T,
    stats: Option[TransformStats],
  ): Unit = {
    stats.foreach(_.record(ruleName))
    println(s"[+] $ruleName")
    println(s"    before: $before")
    println(s"    after:  $after")
    println()
  }

  // ---------------------------------------------------------------------------
  // StepRule
  // ---------------------------------------------------------------------------
  private def applyStepRule(
    rule: StepRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = step match {
    case BlockStep(stepBlock) =>
      val processedChildren = stepBlock.rawSteps.flatMap { childStep =>
        val transformedChild = applyStepRule(rule, childStep, ctx, stats)
        tryStepRule(rule, transformedChild, ctx, stats) match {
          case Some(Some(newStep)) => Some(newStep)
          case Some(None)          => None
          case None                => Some(transformedChild)
        }
      }
      BlockStep(processedChildren.stepBlock)

    case IfStep(cond, thenStep, elseStep, elseConfig) =>
      IfStep(
        cond,
        applyStepRule(rule, thenStep, ctx, stats),
        elseStep.map(applyStepRule(rule, _, ctx, stats)),
        elseConfig,
      )
    case RepeatStep(cond, body) =>
      RepeatStep(cond, applyStepRule(rule, body, ctx, stats))
    case ForEachStep(ty, v, expr, forward, body) =>
      ForEachStep(
        ty,
        v,
        expr,
        forward,
        applyStepRule(rule, body, ctx, stats),
      )
    case ForEachIntegerStep(v, l, li, h, hi, a, body) =>
      ForEachIntegerStep(
        v,
        l,
        li,
        h,
        hi,
        a,
        applyStepRule(rule, body, ctx, stats),
      )
    case other =>
      tryStepRule(rule, other, ctx, stats) match {
        case Some(Some(s)) => s
        case Some(None)    => BlockStep(StepBlock(Nil))
        case None          => other
      }
  }

  private def tryStepRule(
    rule: StepRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Option[Option[Step]] =
    Unifier.unify(rule.pattern, step, ctx, rule.predicates).map { bindings =>
      rule.replace.map { template =>
        val result = Substituter.subst(template, bindings)
        onMatch(rule.name, step, result, stats)
        // apply subrules
        rule.subrules.foldLeft(result) { (s, sr) =>
          transformStep(sr, s, ctx, stats)
        }
      }
    }

  // ---------------------------------------------------------------------------
  // ExpressionRule
  // ---------------------------------------------------------------------------
  private def applyExpressionRule(
    rule: ExpressionRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = {
    new LangWalker {
      override def walk(expr: Expression): Expression = {
        Unifier
          .unify(rule.pattern, expr, ctx, rule.predicates)
          .map { bindings =>
            val result = Substituter.subst(rule.replace, bindings)
            onMatch(rule.name, expr, result, stats)
            result
          }
          .getOrElse(super.walk(expr))
      }
    }.walk(step)
  }

  // ---------------------------------------------------------------------------
  // ConditionRule
  // ---------------------------------------------------------------------------
  private def applyConditionRule(
    rule: ConditionRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = {
    new LangWalker {
      override def walk(cond: Condition): Condition = {
        Unifier
          .unify(rule.pattern, cond, ctx, rule.predicates)
          .map { bindings =>
            val result = Substituter.subst(rule.replace, bindings)
            onMatch(rule.name, cond, result, stats)
            result
          }
          .getOrElse(super.walk(cond))
      }
    }.walk(step)
  }

  // ---------------------------------------------------------------------------
  // ReferenceRule
  // ---------------------------------------------------------------------------
  private def applyReferenceRule(
    rule: ReferenceRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = {
    new LangWalker {
      override def walk(ref: Reference): Reference = {
        Unifier
          .unify(rule.pattern, ref, ctx, rule.predicates)
          .map { bindings =>
            val result = Substituter.subst(rule.replace, bindings)
            onMatch(rule.name, ref, result, stats)
            result
          }
          .getOrElse(super.walk(ref))
      }
    }.walk(step)
  }

  // ---------------------------------------------------------------------------
  // StepBlockRule
  // ---------------------------------------------------------------------------
  private def applyStepBlockRule(
    rule: StepBlockRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = step match {
    case BlockStep(stepBlock) =>
      val newSteps =
        matchSequence(rule, stepBlock.rawSteps, ctx, stats)
      BlockStep(newSteps.stepBlock)
    case IfStep(cond, thenStep, elseStep, elseConfig) =>
      IfStep(
        cond,
        applyStepBlockRule(rule, thenStep, ctx, stats),
        elseStep.map(applyStepBlockRule(rule, _, ctx, stats)),
        elseConfig,
      )
    case RepeatStep(cond, body) =>
      RepeatStep(cond, applyStepBlockRule(rule, body, ctx, stats))
    case ForEachStep(ty, v, expr, forward, body) =>
      ForEachStep(
        ty,
        v,
        expr,
        forward,
        applyStepBlockRule(rule, body, ctx, stats),
      )
    case ForEachIntegerStep(v, l, li, h, hi, a, body) =>
      ForEachIntegerStep(
        v,
        l,
        li,
        h,
        hi,
        a,
        applyStepBlockRule(rule, body, ctx, stats),
      )
    case other => other
  }

  private def matchSequence(
    rule: StepBlockRule,
    steps: List[Step],
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): List[Step] = {
    val window = rule.patternSteps.length
    if (steps.length < window)
      return steps.map(applyStepBlockRule(rule, _, ctx, stats))

    val patternBlock =
      BlockStep(StepBlock(rule.patternSteps.subSteps))
    val concreteBlock =
      BlockStep(StepBlock(steps.take(window).subSteps))

    Unifier.unify(patternBlock, concreteBlock, ctx, rule.predicates) match {
      case Some(bindings) =>
        val replaced = rule.replace.map { template =>
          val result = Substituter.subst(template, bindings)
          onMatch(rule.name, steps.take(window), result, stats)
          // apply subrules
          rule.subrules.foldLeft(result) { (s, sr) =>
            transformStep(sr, s, ctx, stats)
          }
        }
        replaced ++ matchSequence(rule, steps.drop(window), ctx, stats)
      case None =>
        applyStepBlockRule(rule, steps.head, ctx, stats) ::
        matchSequence(rule, steps.tail, ctx, stats)
    }
  }
}
