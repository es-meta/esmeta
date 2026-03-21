package esmeta.es.util.dsl

import esmeta.lang.*

object Unifier {

  def merge(envs: CaptureEnv*): Option[CaptureEnv] = {
    def mergeEnv(
      env1: CaptureEnv,
      env2: CaptureEnv,
    ): Option[CaptureEnv] = {
      if (env1.keySet.intersect(env2.keySet).exists(k => env1(k) != env2(k)))
        None
      else Some(env1 ++ env2)
    }

    envs.foldLeft(Option(Map.empty[String, LangElem]: CaptureEnv)) {
      case (Some(accEnv), nextEnv) => mergeEnv(accEnv, nextEnv)
      case (None, _)               => None
    }
  }

  def unifyList[T](
    patterns: List[T],
    concretes: List[T],
    ctx: DSLContext,
    preds: Map[String, LangElemPredicate],
    unify: (
      T,
      T,
      DSLContext,
      Map[String, LangElemPredicate],
    ) => Option[CaptureEnv],
  ): Option[CaptureEnv] = {
    if (patterns.length != concretes.length) return None

    patterns.zip(concretes).foldLeft(Option(Map.empty[String, LangElem])) {
      case (Some(acc), (p, c)) =>
        unify(p, c, ctx, preds).flatMap(merge(acc, _))
      case (None, _) => None
    }
  }

  // ---------------------------------------------------------------------------
  // Step unification
  // ---------------------------------------------------------------------------
  def unify(
    pattern: Step,
    concrete: Step,
    ctx: DSLContext,
    preds: Map[String, LangElemPredicate],
  ): Option[CaptureEnv] = (pattern, concrete) match {
    case (MetaStep(name, _), node) => Some(Map(name -> node))
    case (LetStep(pVar, pExpr), LetStep(cVar, cExpr)) =>
      for {
        varEnv <- unify(pVar, cVar, ctx, preds)
        exprEnv <- unify(pExpr, cExpr, ctx, preds)
        merged <- merge(varEnv, exprEnv)
      } yield merged
    case (SetStep(pRef, pExpr), SetStep(cRef, cExpr)) =>
      for {
        refEnv <- unify(pRef, cRef, ctx, preds)
        exprEnv <- unify(pExpr, cExpr, ctx, preds)
        merged <- merge(refEnv, exprEnv)
      } yield merged
    case (PerformStep(pExpr), PerformStep(cExpr)) =>
      unify(pExpr, cExpr, ctx, preds)
    case (AppendStep(pElem, pRef), AppendStep(cElem, cRef)) =>
      for {
        elemEnv <- unify(pElem, cElem, ctx, preds)
        refEnv <- unify(pRef, cRef, ctx, preds)
        merged <- merge(elemEnv, refEnv)
      } yield merged
    case (PrependStep(pElem, pRef), PrependStep(cElem, cRef)) =>
      for {
        elemEnv <- unify(pElem, cElem, ctx, preds)
        refEnv <- unify(pRef, cRef, ctx, preds)
        merged <- merge(elemEnv, refEnv)
      } yield merged
    case (ReturnStep(pExpr), ReturnStep(cExpr)) =>
      unify(pExpr, cExpr, ctx, preds)
    case (AssertStep(pCond), AssertStep(cCond)) =>
      unify(pCond, cCond, ctx, preds)
    case (
          ReplaceStep(pOld, pNew, pRef),
          ReplaceStep(cOld, cNew, cRef),
        ) =>
      for {
        oldEnv <- unify(pOld, cOld, ctx, preds)
        newEnv <- unify(pNew, cNew, ctx, preds)
        refEnv <- unify(pRef, cRef, ctx, preds)
        merged <- merge(oldEnv, newEnv, refEnv)
      } yield merged
    case (
          ForEachStep(_, pVar, pExpr, pForward, pBody),
          ForEachStep(_, cVar, cExpr, cForward, cBody),
        ) if pForward == cForward =>
      for {
        varEnv <- unify(pVar, cVar, ctx, preds)
        exprEnv <- unify(pExpr, cExpr, ctx, preds)
        bodyEnv <- unify(pBody, cBody, ctx, preds)
        merged <- merge(varEnv, exprEnv, bodyEnv)
      } yield merged
    case (
          RepeatStep(pCond, pBody),
          RepeatStep(cCond, cBody),
        ) =>
      for {
        condEnv <- unifyLoopCond(pCond, cCond, ctx, preds)
        bodyEnv <- unify(pBody, cBody, ctx, preds)
        merged <- merge(condEnv, bodyEnv)
      } yield merged
    case (
          IfStep(pCond, pThenStep, pElseStep, _),
          IfStep(cCond, cThenStep, cElseStep, _),
        ) =>
      val elseEnvOpt: Option[CaptureEnv] = (pElseStep, cElseStep) match {
        case (Some(pElse), Some(cElse)) => unify(pElse, cElse, ctx, preds)
        case (None, None)               => Some(Map.empty)
        case (Some(_), None) | (None, Some(_)) => None
      }
      for {
        condEnv <- unify(pCond, cCond, ctx, preds)
        thenEnv <- unify(pThenStep, cThenStep, ctx, preds)
        elseEnv <- elseEnvOpt
        merged <- merge(condEnv, thenEnv, elseEnv)
      } yield merged
    case (BlockStep(StepBlock(pSteps)), BlockStep(StepBlock(cSteps))) =>
      unifySubSteps(pSteps, cSteps, ctx, preds)
    case _ => None
  }

  private def unifySubSteps(
    patterns: List[SubStep],
    concretes: List[SubStep],
    ctx: DSLContext,
    preds: Map[String, LangElemPredicate],
  ): Option[CaptureEnv] = (patterns, concretes) match {
    case (Nil, Nil) => Some(Map.empty)
    case (SubStep(_, MetaStep(name, _)) :: pTail, cRemaining) =>
      val captureSize = cRemaining.length - pTail.length
      if (captureSize < 0) {
        None
      } else {
        val (capturedSteps, cTail) = cRemaining.splitAt(captureSize)
        val capturedBlock = BlockStep(StepBlock(capturedSteps))
        val metaEnv = Map(name -> capturedBlock)
        for {
          tailEnv <- unifySubSteps(pTail, cTail, ctx, preds)
          merged <- merge(metaEnv, tailEnv)
        } yield merged
      }
    case (pHead :: pTail, cHead :: cTail) =>
      for {
        headEnv <- unify(pHead.step, cHead.step, ctx, preds)
        tailEnv <- unifySubSteps(pTail, cTail, ctx, preds)
        merged <- merge(headEnv, tailEnv)
      } yield merged
    case _ => None
  }

  private def unifyLoopCond(
    pattern: RepeatStep.LoopCondition,
    concrete: RepeatStep.LoopCondition,
    ctx: DSLContext,
    preds: Map[String, LangElemPredicate],
  ): Option[CaptureEnv] =
    import RepeatStep.LoopCondition.*
    (pattern, concrete) match {
      case (NoCondition, NoCondition) => Some(Map.empty)
      case (While(pCond), While(cCond)) =>
        unify(pCond, cCond, ctx, preds)
      case (Until(pCond), Until(cCond)) =>
        unify(pCond, cCond, ctx, preds)
      case _ => None
    }

  // ---------------------------------------------------------------------------
  // Reference unification
  // ---------------------------------------------------------------------------
  def unify(
    pattern: Reference,
    concrete: Reference,
    ctx: DSLContext,
    preds: Map[String, LangElemPredicate],
  ): Option[CaptureEnv] = (pattern, concrete) match {
    case (MetaReference(name), node) =>
      val passes = preds.get(name).forall(pred => pred(node, ctx))
      if (passes) Some(Map(name -> node)) else None
    case (Variable(name, _), node) if name.startsWith("$") =>
      val passes = preds.get(name).forall(pred => pred(node, ctx))
      if (passes) Some(Map(name -> node)) else None
    case (Variable(p, _), Variable(c, _)) if p == c =>
      Some(Map.empty)
    case (
          Access(pBase, pName, _, _),
          Access(cBase, cName, _, _),
        ) if pName == cName =>
      unify(pBase, cBase, ctx, preds)
    case (
          IndexLookup(pBase, pIndex),
          IndexLookup(cBase, cIndex),
        ) =>
      for {
        baseEnv <- unify(pBase, cBase, ctx, preds)
        indexEnv <- unify(pIndex, cIndex, ctx, preds)
        merged <- merge(baseEnv, indexEnv)
      } yield merged
    case _ => None
  }

  // ---------------------------------------------------------------------------
  // Expression unification
  // ---------------------------------------------------------------------------
  def unify(
    pattern: Expression,
    concrete: Expression,
    ctx: DSLContext,
    preds: Map[String, LangElemPredicate],
  ): Option[CaptureEnv] = (pattern, concrete) match {
    case (MetaExpression(name), node) =>
      val passes = preds.get(name).forall(pred => pred(node, ctx))
      if (passes) Some(Map(name -> node)) else None
    case (
          ListExpression(ListExpressionForm.EmptyList(_, _)),
          ListExpression(ListExpressionForm.EmptyList(_, _)),
        ) =>
      Some(Map.empty)
    case (ListCopyExpression(pExpr), ListCopyExpression(cExpr)) =>
      unify(pExpr, cExpr, ctx, preds)
    case (
          NumberOfExpression(_, _, pExpr, _),
          NumberOfExpression(_, _, cExpr, _),
        ) =>
      unify(pExpr, cExpr, ctx, preds)
    case (
          RecordExpression(pName, pFields, _),
          RecordExpression(cName, cFields, _),
        ) if pName == cName && pFields.length == cFields.length =>
      pFields
        .zip(cFields)
        .foldLeft(Option(Map.empty[String, LangElem])) {
          case (Some(acc), ((pField, pExpr), (cField, cExpr)))
              if pField == cField =>
            for {
              exprEnv <- unify(pExpr, cExpr, ctx, preds)
              merged <- merge(acc, exprEnv)
            } yield merged
          case _ => None
        }
    case (
          InvokeAbstractOperationExpression(pName, pArgs, _),
          InvokeAbstractOperationExpression(cName, cArgs, _),
        ) if pName == cName =>
      unifyList(pArgs, cArgs, ctx, preds, unify)
    case (
          AbstractClosureExpression(pParams, _, pBody),
          AbstractClosureExpression(cParams, _, cBody),
        ) if pParams.length == cParams.length =>
      for {
        paramsEnv <- unifyList(
          pParams,
          cParams,
          ctx,
          preds,
          (
            p: Variable,
            c: Variable,
            ctx: DSLContext,
            preds: Map[
              String,
              LangElemPredicate,
            ],
          ) => unify(p, c, ctx, preds),
        )
        bodyEnv <- unify(pBody, cBody, ctx, preds)
        merged <- merge(paramsEnv, bodyEnv)
      } yield merged
    case (ReferenceExpression(pRef), ReferenceExpression(cRef)) =>
      unify(pRef, cRef, ctx, preds)
    case (
          ReturnIfAbruptExpression(pExpr, pCheck),
          ReturnIfAbruptExpression(cExpr, cCheck),
        ) if pCheck == cCheck =>
      unify(pExpr, cExpr, ctx, preds)
    case (
          BinaryExpression(pLeft, pOp, pRight),
          BinaryExpression(cLeft, cOp, cRight),
        ) if pOp == cOp =>
      for {
        lEnv <- unify(pLeft, cLeft, ctx, preds)
        rEnv <- unify(pRight, cRight, ctx, preds)
        merged <- merge(lEnv, rEnv)
      } yield merged
    case (p: Literal, c: Literal) if p == c => Some(Map.empty)
    case _                                  => None
  }

  // helper to unify Variables as References
  private def unify(
    pattern: Variable,
    concrete: Variable,
    ctx: DSLContext,
    preds: Map[String, LangElemPredicate],
  ): Option[CaptureEnv] =
    unify(pattern: Reference, concrete: Reference, ctx, preds)

  // ---------------------------------------------------------------------------
  // Condition unification
  // ---------------------------------------------------------------------------
  def unify(
    pattern: Condition,
    concrete: Condition,
    ctx: DSLContext,
    preds: Map[String, LangElemPredicate],
  ): Option[CaptureEnv] = (pattern, concrete) match {
    case (MetaCondition(name), node) => Some(Map(name -> node))
    case (CompoundCondition(pl, po, pr), CompoundCondition(cl, co, cr))
        if po == co =>
      for {
        lEnv <- unify(pl, cl, ctx, preds)
        rEnv <- unify(pr, cr, ctx, preds)
        merged <- merge(lEnv, rEnv)
      } yield merged
    case (IsAreCondition(pl, pn, pr), IsAreCondition(cl, cn, cr))
        if pl.length == cl.length && pn == cn && pr.length == cr.length =>
      for {
        lEnv <- unifyList(pl, cl, ctx, preds, unify)
        rEnv <- unifyList(pr, cr, ctx, preds, unify)
        merged <- merge(lEnv, rEnv)
      } yield merged
    case (
          BinaryCondition(pLeft, pOp, pRight),
          BinaryCondition(cLeft, cOp, cRight),
        ) if pOp == cOp =>
      for {
        lEnv <- unify(pLeft, cLeft, ctx, preds)
        rEnv <- unify(pRight, cRight, ctx, preds)
        merged <- merge(lEnv, rEnv)
      } yield merged
    case (
          PredicateCondition(pExpr, pNeg, pOp),
          PredicateCondition(cExpr, cNeg, cOp),
        ) if pNeg == cNeg && pOp == cOp =>
      unify(pExpr, cExpr, ctx, preds)
    case _ => None
  }
}
