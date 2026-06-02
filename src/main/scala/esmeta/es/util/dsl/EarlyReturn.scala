package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.Walker as LangWalker

import AstExtensions.*

/** Preserves outer returns when a rule lifts a matched step body into an
  * abstract closure.
  */
object EarlyReturn {

  private val resultName = "_earlyReturn"

  private sealed trait InvokeSite {
    def closureIndices: Set[Int]
  }
  private case class PerformSite(closureIndices: Set[Int]) extends InvokeSite
  private case class LetSite(closureIndices: Set[Int]) extends InvokeSite

  private case class RuleAnalysis(
    moved: Set[CaptureKey],
    site: Option[InvokeSite],
  )

  def needsWrapping(rule: StepRule, bindings: CaptureEnv): Boolean = {
    val analysis = analyze(rule)
    hasReturningMovedBody(analysis.moved, bindings)
  }

  def wrapIfNeeded(
    rule: StepRule,
    bindings: CaptureEnv,
    step: Step,
  ): Option[Step] = {
    val analysis = analyze(rule)
    if (!hasReturningMovedBody(analysis.moved, bindings)) Some(step)
    else analysis.site.flatMap(wrapSite(step, _))
  }

  private def analyze(rule: StepRule): RuleAnalysis = {
    val moved = movedIntoClosure(rule)
    RuleAnalysis(
      moved = moved,
      site = rule.replace.flatMap(replacementInvokeSite(_, moved)),
    )
  }

  private def hasReturningMovedBody(
    moved: Set[CaptureKey],
    bindings: CaptureEnv,
  ): Boolean =
    moved.exists(key =>
      bindings
        .get(key)
        .exists {
          case step: Step => hasEscapingReturn(step)
          case _          => false
        },
    )

  private def movedIntoClosure(rule: StepRule): Set[CaptureKey] = {
    val patternDepths = metaStepDepths(rule.pattern)
    val replaceDepths = rule.replace.map(metaStepDepths).getOrElse(Map.empty)

    patternDepths.collect {
      case (key, depths)
          if depths.exists(_ == 0) &&
          replaceDepths.get(key).exists(_.exists(_ > 0)) =>
        key
    }.toSet
  }

  private def metaStepDepths(step: Step): Map[CaptureKey, Set[Int]] = {
    var result = Map.empty[CaptureKey, Set[Int]]
    var depth = 0

    def add(key: CaptureKey, depth: Int): Unit =
      result = result.updated(key, result.getOrElse(key, Set.empty) + depth)

    new LangWalker {
      override def walk(step: Step): Step = step match
        case MetaStep(name, _, variant) =>
          add(CaptureKey(name, variant), depth)
          step
        case _ => super.walk(step)

      override def walk(multi: MultilineExpression): MultilineExpression =
        multi match
          case AbstractClosureExpression(params, captured, body) =>
            val saved = depth
            depth = saved + 1
            val walked = AbstractClosureExpression(
              walkList(params, walk),
              walkList(captured, walk),
              walk(body),
            )
            depth = saved
            walked
    }.walk(step)
    result
  }

  private def replacementInvokeSite(
    step: Step,
    moved: Set[CaptureKey],
  ): Option[InvokeSite] = {
    def closureIndices(args: List[Expression]): Set[Int] =
      args.zipWithIndex.collect {
        case (AbstractClosureExpression(_, _, body), index)
            if containsMovedMetaStep(body, moved) =>
          index
      }.toSet

    step match
      case BlockStep(StepBlock(List(SubStep(_, inner)))) =>
        replacementInvokeSite(inner, moved)
      case PerformStep(InvokeAbstractOperationExpression(_, args, _)) =>
        val indices = closureIndices(args)
        Option.when(indices.nonEmpty)(PerformSite(indices))
      case LetStep(_, InvokeAbstractOperationExpression(_, args, _)) =>
        val indices = closureIndices(args)
        Option.when(indices.nonEmpty)(LetSite(indices))
      case _ => None
  }

  private def containsMovedMetaStep(
    step: Step,
    moved: Set[CaptureKey],
  ): Boolean = {
    var found = false
    new LangWalker {
      override def walk(step: Step): Step = step match
        case MetaStep(name, _, variant)
            if moved.contains(CaptureKey(name, variant)) =>
          found = true
          step
        case _ if found => step
        case _          => super.walk(step)

      override def walk(multi: MultilineExpression): MultilineExpression =
        multi
    }.walk(step)
    found
  }

  private def hasEscapingReturn(step: Step): Boolean = {
    var found = false
    new LangWalker {
      override def walk(step: Step): Step = step match
        case ReturnStep(_) =>
          found = true
          step
        case _ if found => step
        case _          => super.walk(step)

      override def walk(multi: MultilineExpression): MultilineExpression =
        multi
    }.walk(step)
    found
  }

  private def patchReturns(step: Step): Step =
    new LangWalker {
      override def walk(step: Step): Step = step match
        case ReturnStep(expr) =>
          ReturnStep(
            RecordExpression(
              "",
              List(
                FieldLiteral("Type") -> EnumLiteral("early-return"),
                FieldLiteral("Value") -> expr,
              ),
              RecordExpressionForm.SyntaxLiteral(None),
            ),
          )
        case _ => super.walk(step)

      override def walk(multi: MultilineExpression): MultilineExpression =
        multi
    }.walk(step)

  private def wrapSite(step: Step, site: InvokeSite): Option[Step] =
    (step, site) match {
      case (
            PerformStep(InvokeAbstractOperationExpression(name, args, tag)),
            PerformSite(indices),
          ) =>
        wrapPerform(name, args, tag, indices, freshResultVariable(step))
      case (
            LetStep(
              variable,
              InvokeAbstractOperationExpression(name, args, tag),
            ),
            LetSite(indices),
          ) =>
        wrapLet(variable, name, args, tag, indices)
      case (BlockStep(StepBlock(List(SubStep(_, inner)))), _) =>
        wrapSite(inner, site)
      case _ => None
    }

  private def wrapPerform(
    name: String,
    args: List[Expression],
    tag: HtmlTag,
    closureIndices: Set[Int],
    result: Variable,
  ): Option[Step] =
    patchClosureArgs(args, closureIndices).map { patchedArgs =>
      val invoke = InvokeAbstractOperationExpression(name, patchedArgs, tag)
      List(
        LetStep(result, invoke),
        earlyReturnCheck(result),
      ).blockStep
    }

  private def wrapLet(
    variable: Variable,
    name: String,
    args: List[Expression],
    tag: HtmlTag,
    closureIndices: Set[Int],
  ): Option[Step] =
    patchClosureArgs(args, closureIndices).map { patchedArgs =>
      val invoke = InvokeAbstractOperationExpression(name, patchedArgs, tag)
      List(
        LetStep(variable, invoke),
        earlyReturnCheck(variable),
      ).blockStep
    }

  private def patchClosureArgs(
    args: List[Expression],
    closureIndices: Set[Int],
  ): Option[List[Expression]] =
    closureIndices.toList.sorted.foldLeft(Option(args)) {
      case (Some(patched), index) if patched.indices.contains(index) =>
        patched(index) match
          case AbstractClosureExpression(params, captured, body) =>
            Some(
              patched.updated(
                index,
                AbstractClosureExpression(params, captured, patchReturns(body)),
              ),
            )
          case _ => None
      case _ => None
    }

  private def earlyReturnCheck(result: Variable): IfStep =
    IfStep(
      CompoundCondition(
        BinaryCondition(
          ReferenceExpression(result),
          BinaryConditionOperator.NEq,
          UndefinedLiteral(),
        ),
        CompoundConditionOperator.And,
        BinaryCondition(
          ReferenceExpression(
            Access(result, "Type", AccessKind.Field, AccessForm.Dot),
          ),
          BinaryConditionOperator.Eq,
          EnumLiteral("early-return"),
        ),
      ),
      ReturnStep(
        ReferenceExpression(
          Access(result, "Value", AccessKind.Field, AccessForm.Dot),
        ),
      ),
      None,
      IfStep.ElseConfig(),
    )

  private def freshResultVariable(step: Step): Variable = {
    val used = variableNames(step)
    val name =
      if (!used.contains(resultName)) resultName
      else
        LazyList
          .from(1)
          .map(i => s"${resultName}$i")
          .find(name => !used.contains(name))
          .get
    Variable(name)
  }

  private def variableNames(step: Step): Set[String] = {
    var names = Set.empty[String]
    new LangWalker {
      override def walk(variable: Variable): Variable = {
        names += variable.name
        super.walk(variable)
      }
    }.walk(step)
    names
  }
}
