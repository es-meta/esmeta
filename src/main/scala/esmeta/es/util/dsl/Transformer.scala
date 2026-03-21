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
      case wr: WhereRule      => applyWhereRule(wr, step, ctx, stats)
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
  // StepRule: uses LangWalker for full AST recursion
  // ---------------------------------------------------------------------------
  private def applyStepRule(
    rule: StepRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = {
    new LangWalker {
      override def walk(sb: StepBlock): StepBlock = {
        val processedChildren = sb.rawSteps.flatMap { childStep =>
          val walked = walk(childStep)
          tryStepRule(rule, walked, ctx, stats) match {
            case Some(Some(newStep)) => Some(newStep)
            case Some(None)          => None
            case None                => Some(walked)
          }
        }
        StepBlock(processedChildren.subSteps)
      }
    }.walk(step)
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
        // apply static + dynamic subrules
        val allSubrules = rule.subrules ++ rule.dynamicSubrules(bindings)
        allSubrules.foldLeft(result) { (s, sr) =>
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
  ): Step = {
    new LangWalker {
      override def walk(sb: StepBlock): StepBlock = {
        val walked = sb.rawSteps.map(walk)
        val matched = matchSequence(rule, walked, ctx, stats)
        StepBlock(matched.subSteps)
      }
    }.walk(step)
  }

  private def matchSequence(
    rule: StepBlockRule,
    steps: List[Step],
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): List[Step] = {
    val window = rule.patternSteps.length
    if (steps.length < window) return steps

    val patternBlock =
      BlockStep(StepBlock(rule.patternSteps.subSteps))
    val concreteBlock =
      BlockStep(StepBlock(steps.take(window).subSteps))

    Unifier.unify(patternBlock, concreteBlock, ctx, rule.predicates) match {
      case Some(bindings) =>
        onMatch(rule.name, steps.take(window), bindings, stats)
        val replaced = rule.dynamicReplace match {
          case Some(fn) => fn(bindings, ctx, stats)
          case None =>
            rule.replace.map { template =>
              val result = Substituter.subst(template, bindings)
              // apply subrules
              rule.subrules.foldLeft(result) { (s, sr) =>
                transformStep(sr, s, ctx, stats)
              }
            }
        }
        replaced ++ matchSequence(rule, steps.drop(window), ctx, stats)
      case None =>
        steps.head :: matchSequence(rule, steps.tail, ctx, stats)
    }
  }

  // ---------------------------------------------------------------------------
  // WhereRule: scan StepBlock for context step, apply rules to siblings
  // ---------------------------------------------------------------------------
  private def applyWhereRule(
    rule: WhereRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = {
    new LangWalker {
      override def walk(sb: StepBlock): StepBlock = {
        val walked = sb.rawSteps.map(walk)
        val processed = whereProcess(rule, walked, ctx, stats)
        StepBlock(processed.subSteps)
      }
    }.walk(step)
  }

  private def whereProcess(
    rule: WhereRule,
    steps: List[Step],
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): List[Step] = steps match {
    case Nil => Nil
    case head :: tail =>
      Unifier.unify(rule.wherePattern, head, ctx, rule.predicates) match {
        case Some(bindings) =>
          onMatch(rule.name, head, head, stats)
          val generatedRules = rule.mainRules(bindings)
          val transformedTail = generatedRules.foldLeft(tail) {
            (steps, genRule) =>
              steps.map(s => transformStep(genRule, s, ctx, stats))
          }
          head :: whereProcess(rule, transformedTail, ctx, stats)
        case None =>
          head :: whereProcess(rule, tail, ctx, stats)
      }
  }
}
