package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.Walker as LangWalker

import AstExtensions.*

object Transformer {

  /** Apply a single rule to an annotated AST tree, returning a plain Step. */
  def transformStep(
    rule: Rule[LangElem],
    root: AStep,
    stats: Option[TransformStats] = None,
  ): Step =
    println(
      s"[TRANSFORM] applying rule '${rule.name}' (${rule.getClass.getSimpleName})",
    )
    rule match {
      case sr: StepRule       => applyStepRule(sr, root, stats)
      case er: ExpressionRule => applyExpressionRule(er, root, stats)
      case cr: ConditionRule  => applyConditionRule(cr, root, stats)
      case rr: ReferenceRule  => applyReferenceRule(rr, root, stats)
    }

  private def onMatch[T](
    ruleName: String,
    before: T,
    after: T,
    stats: Option[TransformStats],
    ctx: DSLContext = DSLContext(),
  ): Unit = {
    stats.foreach(_.record(ruleName))
    println(s"  [MATCH] $ruleName")
    println(
      s"    state: ${ctx.symbolicPaths
        .map { case (k, v) => s"$k→${v.mkString(".")}" }
        .mkString(", ")}",
    )
    println(s"    before: $before")
    println(s"    after:  $after")
  }

  /** Apply sub-rules to a step, pre-substituting parent bindings. */
  private def applySubrules(
    subrules: List[Rule[LangElem]],
    bindings: CaptureEnv,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = {
    val concreteRules = subrules.map(Substituter.substRule(_, bindings))
    concreteRules.foldLeft(step) { (s, sr) =>
      transformStepRaw(sr, s, ctx, stats)
    }
  }

  private def finalizeStepRuleMatch(
    rule: StepRule,
    before: Any,
    template: Step,
    bindings: CaptureEnv,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Option[Step] = {
    val result = Substituter.subst(template, bindings)
    val transformed =
      applySubrules(rule.subrules, bindings, result, ctx, stats)
    EarlyReturn.wrapIfNeeded(rule, bindings, transformed) match {
      case Some(finalStep) =>
        onMatch(rule.name, before, finalStep, stats, ctx)
        Some(finalStep)
      case None =>
        println(
          s"  [WARN] ${rule.name}: closure-lifted return has unsupported replacement shape; skipping match",
        )
        None
    }
  }

  /** Apply a rule to a plain Step (no annotated AST — uses given ctx). */
  private def transformStepRaw(
    rule: Rule[LangElem],
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step =
    rule match {
      case sr: StepRule =>
        applyStepRuleRaw(sr, step, ctx, stats)
      case er: ExpressionRule =>
        applyExpressionRuleRaw(er, step, ctx, stats)
      case cr: ConditionRule =>
        applyConditionRuleRaw(cr, step, ctx, stats)
      case rr: ReferenceRule =>
        applyReferenceRuleRaw(rr, step, ctx, stats)
    }

  // ---------------------------------------------------------------------------
  // StepRule: walk annotated AST bottom-up
  // ---------------------------------------------------------------------------
  private def applyStepRule(
    rule: StepRule,
    root: AStep,
    stats: Option[TransformStats],
  ): Step = {
    val isContextMatch = rule.replace.isEmpty && rule.subrules.nonEmpty

    def walk(astep: AStep): Step = {
      val ctx = DSLContext(symbolicPaths = astep.state)

      if (rule.isMultiStepRule) {
        val walked = rebuildWithWalkedChildren(astep, walk)
        walked match {
          case BlockStep(block) =>
            matchSequence(rule, block.rawSteps, astep.children, stats).blockStep
          case _ => walked
        }
      } else if (isContextMatch) {
        astep.step match {
          case BlockStep(_) =>
            contextMatchBlock(rule, astep.children, stats, walk).blockStep
          case _ =>
            rebuildWithWalkedChildren(astep, walk)
        }
      } else {
        val walked = rebuildWithWalkedChildren(astep, walk)
        tryStepRule(rule, walked, ctx, stats).getOrElse(walked)
      }
    }

    walk(root)
  }

  /** Process block children for context-match rules (replace=None + subrules).
    * When a child matches the pattern, capture bindings and apply subrules to
    * all subsequent siblings.
    */
  private def contextMatchBlock(
    rule: StepRule,
    asteps: List[AStep],
    stats: Option[TransformStats],
    walk: AStep => Step,
  ): List[Step] = asteps match {
    case Nil => Nil
    case head :: tail =>
      val ctx = DSLContext(symbolicPaths = head.state)
      Unifier.unify(rule.pattern, head.step, ctx, rule.predicates) match {
        case Some(bindings) =>
          val concreteRules =
            rule.subrules.map(Substituter.substRule(_, bindings))
          val walkedTail = tail.map(walk)
          val transformedTail = concreteRules.foldLeft(walkedTail) {
            (steps, sr) =>
              steps.map(s => transformStepRaw(sr, s, ctx, stats))
          }
          walk(head) :: transformedTail
        case None =>
          walk(head) :: contextMatchBlock(rule, tail, stats, walk)
      }
  }

  /** Sliding window matching: try a multi-step rule pattern at every offset
    * within a concrete step list. On match, replace the matched window and
    * continue matching on the remainder.
    */
  private def matchSequence(
    rule: StepRule,
    steps: List[Step],
    achildren: List[AStep],
    stats: Option[TransformStats],
  ): List[Step] = {
    val patternSteps = rule.pattern match {
      case BlockStep(sb) => sb.rawSteps
      case _             => List(rule.pattern)
    }
    val window = patternSteps.length

    def go(remaining: List[Step], aRemaining: List[AStep]): List[Step] = {
      if (remaining.length < window) {
        remaining
      } else {
        println("=" * 80)
        println(patternSteps)
        println(aRemaining.take(window).map(_.prettyPrint()))

        Unifier
          .unify(patternSteps, aRemaining.take(window), rule.predicates)
          .flatMap(Unifier.validateVariants)
          .filter(Unifier.evaluateVariantPredicates(_, rule.predicates))
          .flatMap { unifyResult =>
            rule.replace.flatMap { template =>
              val ctx =
                if (aRemaining.nonEmpty)
                  DSLContext(symbolicPaths = aRemaining.head.state)
                else
                  DSLContext()
              println(
                s"  [SLIDING-MATCH] ${rule.name} at window offset, matched ${window} steps",
              )
              println(
                s"    matched: ${aRemaining.take(window).map(_.step).mkString("; ")}",
              )
              finalizeStepRuleMatch(
                rule,
                rule.pattern,
                template,
                unifyResult.bindings,
                ctx,
                stats,
              ).map(
                _ :: go(
                  remaining.drop(window),
                  aRemaining.drop(window),
                ),
              )
            }
          }
          .getOrElse {
            remaining.head :: go(remaining.tail, aRemaining.drop(1))
          }
      }
    }

    go(steps, achildren)
  }

  /** Sliding window matching for rules applied to plain steps, such as
    * subrules. Unlike annotated matching, all windows use the caller-provided
    * context.
    */
  private def matchSequenceRaw(
    rule: StepRule,
    steps: List[Step],
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): List[Step] = {
    val patternSteps = rule.pattern match {
      case BlockStep(sb) => sb.rawSteps
      case _             => List(rule.pattern)
    }
    val window = patternSteps.length

    def go(remaining: List[Step]): List[Step] = {
      if (remaining.length < window) {
        remaining
      } else {
        val current = remaining.take(window)
        Unifier
          .unifyList(patternSteps, current, ctx, rule.predicates, Unifier.unify)
          .flatMap { bindings =>
            rule.replace.flatMap { template =>
              finalizeStepRuleMatch(
                rule,
                rule.pattern,
                template,
                bindings,
                ctx,
                stats,
              ).map(_ :: go(remaining.drop(window)))
            }
          }
          .getOrElse {
            remaining.head :: go(remaining.tail)
          }
      }
    }

    go(steps)
  }

  private def tryStepRule(
    rule: StepRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Option[Step] =
    Unifier.unify(rule.pattern, step, ctx, rule.predicates).flatMap {
      bindings =>
        rule.replace.flatMap { template =>
          finalizeStepRuleMatch(rule, step, template, bindings, ctx, stats)
        }
    }

  // ---------------------------------------------------------------------------
  // StepRule on plain Step (for subrule application)
  // ---------------------------------------------------------------------------
  private def applyStepRuleRaw(
    rule: StepRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = {
    new LangWalker {
      override def walk(step: Step): Step = {
        val walked = super.walk(step)
        if (rule.isMultiStepRule) {
          walked match {
            case BlockStep(block) =>
              matchSequenceRaw(rule, block.rawSteps, ctx, stats).blockStep
            case _ => walked
          }
        } else {
          tryStepRule(rule, walked, ctx, stats).getOrElse(walked)
        }
      }
    }.walk(step)
  }

  // ---------------------------------------------------------------------------
  // ExpressionRule: walk AStep tree, LangWalker for exprs within each step
  // ---------------------------------------------------------------------------
  private def applyExpressionRule(
    rule: ExpressionRule,
    root: AStep,
    stats: Option[TransformStats],
  ): Step = {
    def walk(astep: AStep): Step = {
      val ctx = DSLContext(symbolicPaths = astep.state)
      val base = rebuildWithWalkedChildren(astep, walk)
      applyExpressionRuleToStep(rule, base, ctx, stats)
    }
    walk(root)
  }

  private def applyExpressionRuleToStep(
    rule: ExpressionRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step =
    new LangWalker {
      override def walk(expr: Expression): Expression =
        Unifier
          .unify(rule.pattern, expr, ctx, rule.predicates)
          .flatMap { bindings =>
            rule.replace.map { tmpl =>
              val result = Substituter.subst(tmpl, bindings)
              onMatch(rule.name, expr, result, stats, ctx)
              result
            }
          }
          .getOrElse(super.walk(expr))
    }.walk(step)

  private def applyExpressionRuleRaw(
    rule: ExpressionRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = applyExpressionRuleToStep(rule, step, ctx, stats)

  // ---------------------------------------------------------------------------
  // ConditionRule
  // ---------------------------------------------------------------------------
  private def applyConditionRule(
    rule: ConditionRule,
    root: AStep,
    stats: Option[TransformStats],
  ): Step = {
    def walk(astep: AStep): Step = {
      val ctx = DSLContext(symbolicPaths = astep.state)
      val base = rebuildWithWalkedChildren(astep, walk)
      applyConditionRuleToStep(rule, base, ctx, stats)
    }
    walk(root)
  }

  private def applyConditionRuleToStep(
    rule: ConditionRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step =
    new LangWalker {
      override def walk(cond: Condition): Condition =
        Unifier
          .unify(rule.pattern, cond, ctx, rule.predicates)
          .flatMap { bindings =>
            rule.replace.map { tmpl =>
              val result = Substituter.subst(tmpl, bindings)
              onMatch(rule.name, cond, result, stats, ctx)
              result
            }
          }
          .getOrElse(super.walk(cond))
    }.walk(step)

  private def applyConditionRuleRaw(
    rule: ConditionRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = applyConditionRuleToStep(rule, step, ctx, stats)

  // ---------------------------------------------------------------------------
  // ReferenceRule
  // ---------------------------------------------------------------------------
  private def applyReferenceRule(
    rule: ReferenceRule,
    root: AStep,
    stats: Option[TransformStats],
  ): Step = {
    def walk(astep: AStep): Step = {
      val ctx = DSLContext(symbolicPaths = astep.state)
      val base = rebuildWithWalkedChildren(astep, walk)
      applyReferenceRuleToStep(rule, base, ctx, stats)
    }
    walk(root)
  }

  private def applyReferenceRuleToStep(
    rule: ReferenceRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step =
    new LangWalker {
      override def walk(ref: Reference): Reference =
        Unifier
          .unify(rule.pattern, ref, ctx, rule.predicates)
          .flatMap { bindings =>
            rule.replace.map { tmpl =>
              val result = Substituter.subst(tmpl, bindings)
              onMatch(rule.name, ref, result, stats, ctx)
              result
            }
          }
          .getOrElse(super.walk(ref))
    }.walk(step)

  private def applyReferenceRuleRaw(
    rule: ReferenceRule,
    step: Step,
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): Step = applyReferenceRuleToStep(rule, step, ctx, stats)

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  /** Rebuild a Step from an AStep, replacing children with walked results. */
  private def rebuildWithWalkedChildren(
    astep: AStep,
    walk: AStep => Step,
  ): Step = astep.step match {
    case BlockStep(StepBlock(_)) =>
      val walkedChildren = astep.children.map(walk)
      BlockStep(StepBlock(walkedChildren.subSteps))
    case IfStep(cond, _, _, hasElse) =>
      val walkedThen = walk(astep.children(0))
      val walkedElse =
        if (astep.children.size > 1) Some(walk(astep.children(1))) else None
      IfStep(cond, walkedThen, walkedElse, hasElse)
    case ForEachStep(ty, v, iter, ascending, _) =>
      ForEachStep(ty, v, iter, ascending, walk(astep.children(0)))
    case ForEachIntegerStep(v, low, lowIncl, high, highIncl, asc, _) =>
      ForEachIntegerStep(
        v,
        low,
        lowIncl,
        high,
        highIncl,
        asc,
        walk(astep.children(0)),
      )
    case ForEachOwnPropertyKeyStep(key, obj, cond, asc, order, _) =>
      ForEachOwnPropertyKeyStep(
        key,
        obj,
        cond,
        asc,
        order,
        walk(astep.children(0)),
      )
    case ForEachParseNodeStep(v, expr, _) =>
      ForEachParseNodeStep(v, expr, walk(astep.children(0)))
    case RepeatStep(cond, _) =>
      RepeatStep(cond, walk(astep.children(0)))
    case _ =>
      astep.step // leaf step, no children to rebuild
  }
}
