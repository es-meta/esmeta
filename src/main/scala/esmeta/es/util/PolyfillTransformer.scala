package esmeta.es.util

import esmeta.es.*
import esmeta.lang.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker, Walker => LangWalker}
import scala.collection.mutable

object PolyfillTransformer {
  def apply(body: Step): Step = {
    // 1. Scan the body to find variables that store SetData
    val setDataVars = searchSetDataVariables(body)
    val isSetDataPredicate = (ref: Reference) => isSetData(ref, setDataVars)

    // 2. Transform concrete operations to abstracted function calls
    val yetRemovedBody = replaceSetDataYets(body)
    val loopTransformedBody =
      transformSetDataLoop(yetRemovedBody, isSetDataPredicate)
    replaceSetDataOperations(loopTransformedBody, isSetDataPredicate)
  }

  // ================================================================================
  // Analysis Helpers
  // ================================================================================

  def searchSetDataVariables(body: Step): Set[String] = {
    val result = mutable.Set[String]()
    new LangUnitWalker {
      override def walk(step: Step): Unit = step match
        case LetStep(
              Variable(v, _),
              ListCopyExpression(
                ReferenceExpression(Access(_, "SetData", _, _)),
              ),
            ) =>
          result.add(v)
        case LetStep(
              Variable(v, _),
              ReferenceExpression(Access(_, "SetData", _, _)),
            ) =>
          result.add(v)
        case SetStep(
              Access(_, "SetData", _, _),
              ReferenceExpression(Variable(v, _)),
            ) =>
          result.add(v)
        case _ => super.walk(step)
    }.walk(body)
    result.toSet
  }

  def isSetData(ref: Reference, setDataVars: Set[String]) = ref match
    case Access(_, "SetData", _, _) => true
    case Variable(v, _)             => setDataVars.contains(v)
    case _                          => false

  // ================================================================================
  // Replace Yets in SetData
  // ================================================================================

  def replaceSetDataYets(body: Step): Step = {
    new LangWalker {
      override def walk(step: Step): Step = step match
        case YetStep(
              YetExpression(
                "Replace the element of _S_.[[SetData]] whose value is _e_ with an element whose value is ~empty~.",
                _,
              ),
            ) =>
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__SetDataRemove",
              List(
                ReferenceExpression(
                  Access(
                    Variable("S"),
                    "SetData",
                    AccessKind.Field,
                    AccessForm.Dot,
                  ),
                ),
                ReferenceExpression(Variable("e")),
              ),
              HtmlTag.None,
            ),
          )
        case _ => super.walk(step)

    }.walk(body)
  }

  // ================================================================================
  // Transform SetData related Loops
  // ================================================================================

  def transformSetDataLoop(
    step: Step,
    isSetData: Reference => Boolean,
  ): Step = {
    new LangWalker {
      override def walk(step: Step): Step = step match
        // ```For each element e of S.[[SetData]], do
        //   If e is not empty and SameValue(e, value) is true```
        // Equivalent to SetDataHas
        case ForEachStep(
              _,
              Variable(elem, _),
              ReferenceExpression(ref),
              true,
              BlockStep(
                StepBlock(
                  List(
                    SubStep(
                      _,
                      IfStep(
                        CompoundCondition(
                          IsAreCondition(
                            List(ReferenceExpression(Variable(loopElemL, _))),
                            true,
                            List(EnumLiteral("empty")),
                          ),
                          CompoundConditionOperator.And,
                          IsAreCondition(
                            List(
                              InvokeAbstractOperationExpression(
                                "SameValue",
                                List(
                                  ReferenceExpression(Variable(loopElemR, _)),
                                  ReferenceExpression(Variable(value, _)),
                                ),
                                _,
                              ),
                            ),
                            false,
                            List(TrueLiteral()),
                          ),
                        ),
                        thenStep,
                        elseStep,
                        elseConfig,
                      ),
                    ),
                  ),
                ),
              ),
            ) if elem == loopElemL && elem == loopElemR && isSetData(ref) =>
          IfStep(
            IsAreCondition(
              List(
                InvokeAbstractOperationExpression(
                  "IN__SetDataHas",
                  List(
                    ReferenceExpression(ref),
                    ReferenceExpression(Variable(value)),
                  ),
                  HtmlTag.None,
                ),
              ),
              false,
              List(TrueLiteral()),
            ),
            replaceVariable(thenStep, elem, value),
            elseStep.map(replaceVariable(_, elem, value)),
            elseConfig,
          )
        case _ => super.walk(step)

      override def walk(stepBlock: StepBlock): StepBlock =
        def walkSubSteps(steps: List[SubStep]): List[SubStep] =
          steps match
            case SubStep(
                  _,
                  LetStep(
                    Variable(lengthInit, _),
                    NumberOfExpression(
                      "elements",
                      _,
                      ReferenceExpression(ref),
                      _,
                    ),
                  ),
                ) ::
                SubStep(
                  _,
                  LetStep(Variable(indexInit, _), DecimalMathValueLiteral(0)),
                ) ::
                SubStep(
                  _,
                  RepeatStep(
                    RepeatStep.LoopCondition.While(
                      BinaryCondition(
                        ReferenceExpression(Variable(indexCond, _)),
                        BinaryConditionOperator.LessThan,
                        ReferenceExpression(Variable(lengthCond, _)),
                      ),
                    ),
                    body,
                  ),
                ) :: tail
                if lengthInit == lengthCond && indexInit == indexCond && isSetData(
                  ref,
                ) =>
              // Step 1: Find the base of index and the name of stored variable
              // search(... let e = resultSetData[index]; ..., "index")
              // -> ("e", "resultSetData")
              val (loopBase, loopVar) = searchLoopVariable(body, indexInit)

              // Step 2: Remove every loop-related steps
              val strippedBody = replaceSetDataRemove(
                removeLoopRelatedSteps(body, lengthInit, indexInit),
              )

              // Step 3: Replace the remove/add statement to function call
              // Set resultSetData[index] to ~empty~; -> remove(resultSetData, e);
              // Append e to resultSetData;           -> insert(resultSetData, e);
              val transformedBody =
                replaceLoopVariable(
                  strippedBody,
                  loopBase,
                  loopVar,
                  indexInit,
                )

              val loopWithEarlyReturn = wrapWithEarlyReturn(
                transformedBody,
                "IN__SetDataIterateLoop",
                loopBase,
                loopVar,
              )

              SubStep(None, loopWithEarlyReturn) :: walkSubSteps(tail)
            case SubStep(
                  _,
                  LetStep(
                    Variable(nextInit, _),
                    EnumLiteral("not-started"),
                  ),
                ) ::
                SubStep(
                  _,
                  RepeatStep(
                    RepeatStep.LoopCondition.While(
                      IsAreCondition(
                        List(ReferenceExpression(Variable(nextCond, _))),
                        true,
                        List(EnumLiteral("done")),
                      ),
                    ),
                    body,
                  ),
                ) :: tail if nextInit == nextCond =>
              val iter = searchIteratorVariable(body)

              val strippedBody = replaceSetDataHas(replaceSetDataRemove(body))

              val transformedBody =
                replaceIteratorVariable(strippedBody, nextInit)

              val loopWithEarlyReturn = wrapWithEarlyReturn(
                transformedBody,
                "IN__SetDataIterateIterator",
                iter,
                nextInit,
              )

              SubStep(None, loopWithEarlyReturn) :: walkSubSteps(tail)
            case h :: t => walk(h) :: walkSubSteps(t)
            case Nil    => Nil
        StepBlock(walkSubSteps(stepBlock.steps))
    }.walk(step)
  }

  // ================================================================================
  // Replace SetData related Operations
  // ================================================================================

  def replaceSetDataOperations(
    body: Step,
    isSetData: Reference => Boolean,
  ): Step = {
    new LangWalker {
      override def walk(step: Step): Step = step match
        case SetStep(ref, ListExpression(ListExpressionForm.EmptyList(_, _)))
            if isSetData(ref) =>
          SetStep(
            ref,
            InvokeAbstractOperationExpression(
              "IN__SetDataCreate",
              List(),
              HtmlTag.None,
            ),
          )
        case LetStep(ref, ListExpression(ListExpressionForm.EmptyList(_, _)))
            if isSetData(ref) =>
          LetStep(
            ref,
            InvokeAbstractOperationExpression(
              "IN__SetDataCreate",
              List(),
              HtmlTag.None,
            ),
          )
        case AppendStep(elem, ref) if isSetData(ref) =>
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__SetDataInsert",
              List(
                ReferenceExpression(ref),
                elem,
              ),
              HtmlTag.None,
            ),
          )
        case LetStep(variable, ListCopyExpression(ReferenceExpression(ref)))
            if isSetData(ref) =>
          LetStep(
            variable,
            InvokeAbstractOperationExpression(
              "IN__SetDataCopy",
              List(ReferenceExpression(ref)),
              HtmlTag.None,
            ),
          )
        case _ => super.walk(step)

      override def walk(expr: Expression): Expression = expr match
        case InvokeAbstractOperationExpression(
              "SetDataHas",
              List(ReferenceExpression(ref), elem),
              tag,
            ) if isSetData(ref) =>
          InvokeAbstractOperationExpression(
            "IN__SetDataHas",
            List(ReferenceExpression(ref), elem),
            tag,
          )
        case InvokeAbstractOperationExpression(
              "SetDataSize",
              List(ReferenceExpression(ref)),
              tag,
            ) if isSetData(ref) =>
          InvokeAbstractOperationExpression(
            "IN__SetDataSize",
            List(ReferenceExpression(ref)),
            tag,
          )
        case _ => super.walk(expr)

    }.walk(body)
  }

  // ================================================================================
  // Transformation Helpers
  // ================================================================================

  def searchLoopVariable(
    body: Step,
    index: String,
  ): (Reference, String) = {
    var base: Option[Reference] = None
    var varName: Option[String] = None
    new LangUnitWalker {
      override def walk(step: Step): Unit =
        step match
          case LetStep(
                Variable(x, _),
                ReferenceExpression(
                  IndexLookup(
                    b,
                    ReferenceExpression(Variable(i, _)),
                  ),
                ),
              ) =>
            base = Some(b); varName = Some(x)
          case _ => super.walk(step)
    }.walk(body)

    // Should find the loop variable
    (
      base.getOrElse(throw new Error),
      varName.getOrElse(throw new Error),
    )
  }

  def replaceLoopVariable(
    body: Step,
    base: Reference,
    varName: String,
    index: String,
  ): Step = {
    new LangWalker {
      override def walk(stepBlock: StepBlock): StepBlock =
        def walkSubSteps(steps: List[SubStep]): List[SubStep] =
          steps match
            case SubStep(
                  _,
                  SetStep(
                    IndexLookup(
                      b,
                      ReferenceExpression(Variable(i, _)),
                    ),
                    EnumLiteral("empty"),
                  ),
                ) :: tail if b == base && i == index =>
              SubStep(
                None,
                PerformStep(
                  InvokeAbstractOperationExpression(
                    "IN__SetDataRemove",
                    List(
                      ReferenceExpression(base),
                      ReferenceExpression(Variable(varName, None)),
                    ),
                    HtmlTag.None,
                  ),
                ),
              ) :: walkSubSteps(tail)
            case head :: tail => super.walk(head) :: walkSubSteps(tail)
            case Nil          => Nil
        StepBlock(walkSubSteps(stepBlock.steps))
    }.walk(body)
  }

  def removeLoopRelatedSteps(
    body: Step,
    length: String,
    index: String,
  ): Step = {
    new LangWalker {
      override def walk(stepBlock: StepBlock): StepBlock =
        def walkSubSteps(steps: List[SubStep]): List[SubStep] =
          steps match
            case SubStep(
                  _,
                  LetStep(
                    Variable(x, _),
                    ReferenceExpression(
                      IndexLookup(
                        b,
                        ReferenceExpression(Variable(i, _)),
                      ),
                    ),
                  ),
                ) :: tail if i == index =>
              walkSubSteps(tail)
            case SubStep(_, SetStep(Variable(lLhs, _), _)) :: tail
                if lLhs == length =>
              walkSubSteps(tail)
            case SubStep(
                  _,
                  SetStep(
                    Variable(iLhs, _),
                    BinaryExpression(
                      ReferenceExpression(Variable(iRhs, _)),
                      BinaryExpressionOperator.Add,
                      DecimalMathValueLiteral(1),
                    ),
                  ),
                ) :: tail if iLhs == index && iRhs == index =>
              walkSubSteps(tail)
            case head :: tail => super.walk(head) :: walkSubSteps(tail)
            case Nil          => Nil
        StepBlock(walkSubSteps(stepBlock.steps))
    }.walk(body)
  }

  def searchIteratorVariable(body: Step): Reference = {
    var result: Option[Reference] = None
    new LangUnitWalker {
      override def walk(expr: Expression) = expr match {
        case InvokeAbstractOperationExpression(
              "IteratorStepValue",
              List(ReferenceExpression(iter)),
              _,
            ) =>
          result = Some(iter)
        case _ => super.walk(expr)
      }
    }.walk(body)
    result.getOrElse(throw new Error)
  }

  def replaceIteratorVariable(
    body: Step,
    varName: String,
  ): Step = {
    new LangWalker {
      override def walk(stepBlock: StepBlock): StepBlock =
        def walkSubSteps(steps: List[SubStep]): List[SubStep] =
          steps match
            case SubStep(_, SetStep(Variable(iLhs, _), _)) :: tail
                if iLhs == varName =>
              walkSubSteps(tail)
            case head :: tail => super.walk(head) :: walkSubSteps(tail)
            case Nil          => Nil
        StepBlock(walkSubSteps(stepBlock.steps))
    }.walk(body)
  }

  def replaceSetDataRemove(body: Step): Step = {
    new LangWalker {
      override def walk(stepBlock: StepBlock): StepBlock =
        def walkSubSteps(steps: List[SubStep]): List[SubStep] =
          steps match
            case SubStep(
                  d,
                  LetStep(
                    index,
                    InvokeAbstractOperationExpression(
                      "SetDataIndex",
                      List(
                        ReferenceExpression(base),
                        ReferenceExpression(elem),
                      ),
                      t,
                    ),
                  ),
                ) :: tail =>
              SubStep(
                d,
                LetStep(
                  index,
                  InvokeAbstractOperationExpression(
                    "SetDataIndex",
                    List(
                      ReferenceExpression(base),
                      ReferenceExpression(elem),
                    ),
                    t,
                  ),
                ),
              ) :: tail.map {
                new LangWalker {
                  override def walk(step: Step): Step = step match
                    case SetStep(
                          IndexLookup(b, ReferenceExpression(i)),
                          EnumLiteral("empty"),
                        ) if b == base && i == index =>
                      PerformStep(
                        InvokeAbstractOperationExpression(
                          "IN__SetDataRemove",
                          List(
                            ReferenceExpression(base),
                            ReferenceExpression(elem),
                          ),
                          HtmlTag.None,
                        ),
                      )
                    case _ => super.walk(step)
                }.walk(_)
              }
            case head :: tail => super.walk(head) :: walkSubSteps(tail)
            case Nil          => Nil
        StepBlock(walkSubSteps(stepBlock.steps))
    }.walk(body)
  }

  def replaceSetDataHas(body: Step): Step = {
    new LangWalker {
      override def walk(stepBlock: StepBlock): StepBlock =
        def walkSubSteps(steps: List[SubStep]): List[SubStep] =
          steps match
            case SubStep(
                  d,
                  LetStep(
                    index,
                    InvokeAbstractOperationExpression(
                      "SetDataIndex",
                      List(
                        ReferenceExpression(base),
                        ReferenceExpression(elem),
                      ),
                      t,
                    ),
                  ),
                ) :: tail =>
              tail.map {
                new LangWalker {
                  override def walk(cond: Condition): Condition = cond match
                    case IsAreCondition(
                          List(ReferenceExpression(i)),
                          neg,
                          List(EnumLiteral("not-found")),
                        ) if i == index =>
                      IsAreCondition(
                        List(
                          InvokeAbstractOperationExpression(
                            "IN__SetDataHas",
                            List(
                              ReferenceExpression(base),
                              ReferenceExpression(elem),
                            ),
                            HtmlTag.None,
                          ),
                        ),
                        neg,
                        List(FalseLiteral()),
                      )
                    case _ => super.walk(cond)
                }.walk(_)
              }
            case head :: tail => super.walk(head) :: walkSubSteps(tail)
            case Nil          => Nil
        StepBlock(walkSubSteps(stepBlock.steps))
    }.walk(body)
  }

  def wrapWithEarlyReturn(
    body: Step,
    aoName: String,
    iterBase: Reference,
    elementVar: String,
  ): Step = {

    def searchEarlyReturn(body: LangElem): Option[Expression] = {
      var result: Option[Expression] = None
      new LangUnitWalker {
        override def walk(step: Step) = step match {
          case ReturnStep(expr) => result = Some(expr)
          case _                => super.walk(step)
        }
      }.walk(body)
      result
    }

    def replaceReturnStep(body: Step, resultExpr: Expression): Step = {
      new LangWalker {
        override def walk(step: Step): Step = step match
          case ReturnStep(expr) if expr == resultExpr =>
            ReturnStep(EnumLiteral("early-return"))
          case _ => super.walk(step)
      }.walk(body)
    }

    searchEarlyReturn(body) match {
      case Some(expr) =>
        val returnReplacedBody = replaceReturnStep(body, expr)
        val bodyWithEarlyReturnCheck =
          InvokeAbstractOperationExpression(
            aoName,
            List(
              ReferenceExpression(iterBase),
              AbstractClosureExpression(
                List(Variable(elementVar)),
                List(),
                returnReplacedBody,
              ),
              TrueLiteral(),
            ),
            HtmlTag.None,
          )
        IfStep(
          IsAreCondition(
            List(bodyWithEarlyReturnCheck),
            false,
            List(EnumLiteral("early-return")),
          ),
          ReturnStep(expr),
          None,
          IfStep.ElseConfig(false, "", false),
        )
      case None =>
        PerformStep(
          InvokeAbstractOperationExpression(
            aoName,
            List(
              ReferenceExpression(iterBase),
              AbstractClosureExpression(
                List(Variable(elementVar)),
                List(),
                body,
              ),
            ),
            HtmlTag.None,
          ),
        )
    }
  }

  def replaceVariable(body: Step, from: String, to: String): Step = {
    new LangWalker {
      override def walk(ref: Reference): Reference = ref match
        case Variable(x, nt) if x == from => Variable(to, nt)
        case _                            => super.walk(ref)
    }.walk(body)
  }
}
