package esmeta.es.util

import esmeta.es.*
import esmeta.lang.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker, Walker => LangWalker}
import esmeta.spec.*
import esmeta.util.BaseUtils.*

import scala.collection.mutable
import scala.annotation.tailrec

object DSLPath extends OptimizationPath {
  val transform: List[Algorithm] => List[Algorithm] =
    (MapDataTransformer(_)) andThen
    (SetDataTransformer(_))
  // andThen
  // (InternalSlotTransformer(_))

  def apply(body: List[Algorithm]) = transform(body)
}

case class TransformationRule(
  name: String,
  stepPattern: PartialFunction[Step, TransformationRuleEngine => Step] =
    PartialFunction.empty,
  stepBlockPattern: PartialFunction[
    StepBlock,
    TransformationRuleEngine => StepBlock,
  ] = PartialFunction.empty,
  expressionPattern: PartialFunction[
    Expression,
    TransformationRuleEngine => Expression,
  ] = PartialFunction.empty,
  conditionPattern: PartialFunction[
    Condition,
    TransformationRuleEngine => Condition,
  ] = PartialFunction.empty,
  referencePattern: PartialFunction[
    Reference,
    TransformationRuleEngine => Reference,
  ] = PartialFunction.empty,
)

class TransformationRuleEngine(title: String) {
  private val counts = mutable.Map[String, Int]().withDefaultValue(0)

  def recordMatch(ruleName: String): Unit = counts(ruleName) += 1

  def printSummary(): Unit = {
    println(s"\n=== $title Transformation Summary ===")
    counts.toList.sortBy(_._1).foreach {
      case (name, count) =>
        println(f"$name%-45s : $count%3d changes")
    }
    println("-" * 65)

    val total = counts.values.foldLeft(0)(_ + _)
    println(f"${"Total"}%-45s : $total%3d changes\n")
  }

  def transformStep(step: Step, rules: List[TransformationRule]): Step =
    rules.foldLeft(step) { (currentStep, rule) =>
      applyRuleToNode(currentStep, rule)
    }

  def transformStepBlock(
    stepBlock: StepBlock,
    rules: List[TransformationRule],
  ): StepBlock =
    rules.foldLeft(stepBlock) { (currentBlock, rule) =>
      applyRuleToNode(currentBlock, rule)
    }

  private def applyRuleToNode[T <: LangElem](
    node: T,
    rule: TransformationRule,
  ): T = {
    val walker = new LangWalker {
      override def walk(s: Step): Step = {
        if (rule.stepPattern.isDefinedAt(s)) {
          recordMatch(rule.name)
          rule.stepPattern(s)(TransformationRuleEngine.this)
        } else super.walk(s)
      }
      override def walk(sb: StepBlock): StepBlock = {
        if (rule.stepBlockPattern.isDefinedAt(sb)) {
          recordMatch(rule.name)
          rule.stepBlockPattern(sb)(TransformationRuleEngine.this)
        } else super.walk(sb)
      }
      override def walk(e: Expression): Expression = {
        if (rule.expressionPattern.isDefinedAt(e)) {
          recordMatch(rule.name)
          rule.expressionPattern(e)(TransformationRuleEngine.this)
        } else super.walk(e)
      }
      override def walk(c: Condition): Condition = {
        if (rule.conditionPattern.isDefinedAt(c)) {
          recordMatch(rule.name)
          rule.conditionPattern(c)(TransformationRuleEngine.this)
        } else super.walk(c)
      }
      override def walk(r: Reference): Reference = {
        if (rule.referencePattern.isDefinedAt(r)) {
          recordMatch(rule.name)
          rule.referencePattern(r)(TransformationRuleEngine.this)
        } else super.walk(r)
      }
    }
    walker.walk(node).asInstanceOf[T]
  }
}

object MapDataTransformer {
  def apply(targets: List[Algorithm]): List[Algorithm] = {
    val engine = new TransformationRuleEngine("MapData")

    val optimizedTargets = targets.map { algo =>
      val mapDataVars = searchMapDataVariables(algo.body)
      val isMapDataPredicate = (ref: Reference) => isMapData(ref, mapDataVars)

      val rules = List(
        mapDataHasRule(isMapDataPredicate),
        mapDataForEachRule(isMapDataPredicate),
        mapDataWhileRule(isMapDataPredicate),
      ) ++ mapDataOperationsRule(isMapDataPredicate)

      val newBody = engine.transformStep(algo.body, rules)

      algo.copy(body = newBody)
    }

    engine.printSummary()
    optimizedTargets
  }

  // ================================================================================
  // Rules
  // ================================================================================

  def mapDataOperationsRule(
    isMapData: Reference => Boolean,
  ): List[TransformationRule] = List(
    TransformationRule(
      name = "[OPER] MapData Insert",
      stepPattern = {
        case AppendStep(elem, ref) if isMapData(ref) =>
          _ =>
            PerformStep(
              InvokeAbstractOperationExpression(
                "IN__MapDataInsert",
                List(ReferenceExpression(ref), elem),
                HtmlTag.None,
              ),
            )
      },
    ),
    TransformationRule(
      name = "[OPER] MapData Create",
      stepPattern = {
        case SetStep(ref, ListExpression(ListExpressionForm.EmptyList(_, _)))
            if isMapData(ref) =>
          _ =>
            SetStep(
              ref,
              InvokeAbstractOperationExpression(
                "IN__MapDataCreate",
                List(),
                HtmlTag.None,
              ),
            )
      },
    ),
  )
  // 1. For each Record { [[Key]], [[Value]] } p of $base, do
  //     1. If p.[[Key]] is not empty and SameValue(p.[[Key]], $key) is true, then
  // => MapDataHas($base, $key), p = MapDataGet($base, $key)
  def mapDataHasRule(isMapData: Reference => Boolean): TransformationRule =
    TransformationRule(
      name = "MapData Has",
      stepPattern = {
        case ForEachStep(
              _,
              Variable(elem, _),
              ReferenceExpression(base),
              true,
              BlockStep(
                StepBlock(
                  List(
                    SubStep(
                      _,
                      IfStep(
                        CompoundCondition(
                          IsAreCondition(
                            List(
                              ReferenceExpression(
                                Access(Variable(loopElemL, _), "Key", _, _),
                              ),
                            ),
                            true,
                            List(EnumLiteral("empty")),
                          ),
                          CompoundConditionOperator.And,
                          IsAreCondition(
                            List(
                              InvokeAbstractOperationExpression(
                                "SameValue",
                                List(
                                  ReferenceExpression(
                                    Access(Variable(loopElemR, _), "Key", _, _),
                                  ),
                                  ReferenceExpression(Variable(key, _)),
                                ),
                                _,
                              ),
                            ),
                            false,
                            List(TrueLiteral()),
                          ),
                        ),
                        thenStep,
                        None,
                        elseConfig,
                      ),
                    ),
                  ),
                ),
              ),
            ) if elem == loopElemL && elem == loopElemR && isMapData(base) =>
          engine =>
            val thenStepTransformed = engine.transformStep(
              thenStep,
              mapDataHasSubRules(base, elem, key),
            )

            IfStep(
              IsAreCondition(
                List(
                  InvokeAbstractOperationExpression(
                    "IN__MapDataHas",
                    List(
                      ReferenceExpression(base),
                      ReferenceExpression(Variable(key)),
                    ),
                    HtmlTag.None,
                  ),
                ),
                false,
                List(TrueLiteral()),
              ),
              thenStepTransformed,
              None,
              elseConfig,
            )
      },
    )

  def mapDataForEachRule(isMapData: Reference => Boolean): TransformationRule =
    TransformationRule(
      name = "MapData ForEach",
      stepPattern = {
        case ForEachStep(
              _,
              Variable(elem, _),
              ReferenceExpression(ref),
              true,
              body,
            ) if isMapData(ref) =>
          engine =>
            val bodyTransformed = engine.transformStep(
              body,
              subRulesForIterate(ref, elem, "ForEach"),
            )
            PerformStep(
              InvokeAbstractOperationExpression(
                "IN__MapDataIterateForEach",
                List(
                  ReferenceExpression(ref),
                  AbstractClosureExpression(
                    List(Variable(elem)),
                    List(),
                    bodyTransformed,
                  ),
                ),
                HtmlTag.None,
              ),
            )
      },
    )

  def mapDataWhileRule(isMapData: Reference => Boolean): TransformationRule =
    TransformationRule(
      name = "MapData While",
      stepBlockPattern = {
        case StepBlock(steps)
            if containsWhileLoopSequence(steps, isMapData).isDefined =>
          engine =>
            def process(list: List[SubStep]): List[SubStep] = list match {
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
                      loopBody,
                    ),
                  ) :: tail
                  if lengthInit == lengthCond && indexInit == indexCond && isMapData(
                    ref,
                  ) =>
                val (loopBase, loopVar) =
                  searchLoopVariable(loopBody, indexInit)

                val cleanBody = engine.transformStep(
                  loopBody,
                  mapDataIterateWhileSubRules(lengthInit, indexInit),
                )
                val finalBody = engine.transformStep(
                  cleanBody,
                  subRulesForIterate(loopBase, loopVar, "While"),
                )
                SubStep(
                  None,
                  PerformStep(
                    InvokeAbstractOperationExpression(
                      "IN__MapDataIterateWhile",
                      List(
                        ReferenceExpression(loopBase),
                        AbstractClosureExpression(
                          List(Variable(loopVar)),
                          List(),
                          finalBody,
                        ),
                      ),
                      HtmlTag.None,
                    ),
                  ),
                ) :: process(tail)

              case head :: tail =>
                head.copy(step =
                  engine.transformStep(
                    head.step,
                    List(mapDataWhileRule(isMapData)),
                  ),
                ) :: process(tail)
              case Nil => Nil
            }
            StepBlock(process(steps))
      },
    )

  // ================================================================================
  // SubRules
  // ================================================================================

  def mapDataHasSubRules(
    base: Reference,
    elem: String,
    key: String,
  ): List[TransformationRule] =
    List(
      TransformationRule(
        name = "MapData Has >> MapData Remove",
        stepBlockPattern = {
          case StepBlock(steps) if containsRemoveSequence(steps, elem) =>
            engine =>
              def process(list: List[SubStep]): List[SubStep] = list match {
                case SubStep(
                      _,
                      SetStep(
                        Access(Variable(elem1, _), "Key", _, _),
                        EnumLiteral("empty"),
                      ),
                    ) ::
                    SubStep(
                      _,
                      SetStep(
                        Access(Variable(elem2, _), "Value", _, _),
                        EnumLiteral("empty"),
                      ),
                    ) :: tail if elem == elem1 && elem == elem2 =>
                  SubStep(
                    None,
                    PerformStep(
                      InvokeAbstractOperationExpression(
                        "IN__MapDataRemove",
                        List(
                          ReferenceExpression(base),
                          ReferenceExpression(Variable(key)),
                        ),
                        HtmlTag.None,
                      ),
                    ),
                  ) :: process(tail)
                case head :: tail =>
                  head.copy(step =
                    engine.transformStep(
                      head.step,
                      mapDataHasSubRules(base, elem, key),
                    ),
                  ) :: process(tail)
                case Nil => Nil
              }
              StepBlock(process(steps))
        },
      ),
      TransformationRule(
        name = "MapData Has >> MapData Set",
        stepPattern = {
          case SetStep(Access(Variable(elem1, _), "Value", _, _), expr)
              if elem == elem1 =>
            _ =>
              PerformStep(
                InvokeAbstractOperationExpression(
                  "IN__MapDataSet",
                  List(
                    ReferenceExpression(base),
                    ReferenceExpression(Variable(key)),
                    expr,
                  ),
                  HtmlTag.None,
                ),
              )
        },
      ),
      TransformationRule(
        name = "MapData Has >> MapData Get",
        stepPattern = {
          case ReturnStep(
                ReferenceExpression(Access(Variable(elem1, _), "Value", _, _)),
              ) if elem == elem1 =>
            _ =>
              ReturnStep(
                InvokeAbstractOperationExpression(
                  "IN__MapDataGet",
                  List(
                    ReferenceExpression(base),
                    ReferenceExpression(Variable(key)),
                  ),
                  HtmlTag.None,
                ),
              )
        },
      ),
    )

  def mapDataIterateWhileSubRules(
    length: String,
    index: String,
  ): List[TransformationRule] =
    List(
      TransformationRule(
        name = "MapData While >> Remove Index Operations",
        stepBlockPattern = {
          case StepBlock(innerSteps) =>
            innerEngine =>
              def filterSteps(innerList: List[SubStep]): List[SubStep] =
                innerList match {
                  case SubStep(
                        _,
                        LetStep(
                          Variable(_, _),
                          ReferenceExpression(
                            IndexLookup(
                              _,
                              ReferenceExpression(Variable(i, _)),
                            ),
                          ),
                        ),
                      ) :: innerTail if i == index =>
                    filterSteps(innerTail)
                  case SubStep(_, SetStep(Variable(lLhs, _), _)) :: innerTail
                      if lLhs == length =>
                    filterSteps(innerTail)
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
                      ) :: innerTail if iLhs == index && iRhs == index =>
                    filterSteps(innerTail)
                  case head :: innerTail =>
                    head.copy(step =
                      innerEngine.transformStep(
                        head.step,
                        mapDataIterateWhileSubRules(length, index),
                      ),
                    ) :: filterSteps(innerTail)
                  case Nil => Nil
                }
              StepBlock(filterSteps(innerSteps))
        },
      ),
    )

  def subRulesForIterate(
    base: Reference,
    elem: String,
    parentName: String,
  ): List[TransformationRule] = List(
    TransformationRule(
      name = s"MapData $parentName >> Remove Existence Check",
      stepPattern = {
        case IfStep(
              IsAreCondition(
                List(
                  ReferenceExpression(Access(Variable(elem1, _), "Key", _, _)),
                ),
                true,
                List(EnumLiteral("empty")),
              ),
              thenStep,
              None,
              _,
            ) if elem == elem1 =>
          engine =>
            engine.transformStep(
              thenStep,
              subRulesForIterate(base, elem, parentName),
            )
      },
    ),
    TransformationRule(
      name = s"MapData $parentName >> MapData Remove",
      stepBlockPattern = {
        case StepBlock(steps) if containsRemoveSequence(steps, elem) =>
          engine =>
            def process(list: List[SubStep]): List[SubStep] = list match {
              case SubStep(
                    _,
                    SetStep(
                      Access(Variable(elem1, _), "Key", _, _),
                      EnumLiteral("empty"),
                    ),
                  ) ::
                  SubStep(
                    _,
                    SetStep(
                      Access(Variable(elem2, _), "Value", _, _),
                      EnumLiteral("empty"),
                    ),
                  ) :: tail if elem == elem1 && elem == elem2 =>
                SubStep(
                  None,
                  PerformStep(
                    InvokeAbstractOperationExpression(
                      "IN__MapDataRemove",
                      List(
                        ReferenceExpression(base),
                        ReferenceExpression(
                          Access(
                            Variable(elem),
                            "Key",
                            AccessKind.Field,
                            AccessForm.Dot,
                          ),
                        ),
                      ),
                      HtmlTag.None,
                    ),
                  ),
                ) :: process(tail)
              case head :: tail =>
                head.copy(step =
                  engine.transformStep(
                    head.step,
                    subRulesForIterate(base, elem, parentName),
                  ),
                ) :: process(tail)
              case Nil => Nil
            }
            StepBlock(process(steps))
      },
    ),
  )

  // ================================================================================
  // Sequence Finders
  // ================================================================================

  @tailrec
  def containsWhileLoopSequence(
    steps: List[SubStep],
    isMapData: Reference => Boolean,
  ): Option[(String, String, String, String, Reference)] = steps match {
    case SubStep(
          _,
          LetStep(
            Variable(lengthInit, _),
            NumberOfExpression("elements", _, ReferenceExpression(ref), _),
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
            _,
          ),
        ) :: _
        if lengthInit == lengthCond && indexInit == indexCond && isMapData(
          ref,
        ) =>
      Some((lengthInit, indexInit, lengthCond, indexCond, ref))
    case _ :: tail => containsWhileLoopSequence(tail, isMapData)
    case Nil       => None
  }

  @tailrec
  def containsRemoveSequence(steps: List[SubStep], elem: String): Boolean =
    steps match {
      case SubStep(
            _,
            SetStep(
              Access(Variable(elem1, _), "Key", _, _),
              EnumLiteral("empty"),
            ),
          ) ::
          SubStep(
            _,
            SetStep(
              Access(Variable(elem2, _), "Value", _, _),
              EnumLiteral("empty"),
            ),
          ) :: _ if elem == elem1 && elem == elem2 =>
        true
      case _ :: tail => containsRemoveSequence(tail, elem)
      case Nil       => false
    }

  // ================================================================================
  // Analysis Helpers
  // ================================================================================

  def searchMapDataVariables(step: Step): Set[String] = {
    val result = mutable.Set[String]()
    new LangUnitWalker {
      override def walk(s: Step): Unit = s match
        case LetStep(
              Variable(v, _),
              ListCopyExpression(
                ReferenceExpression(Access(_, "MapData", _, _)),
              ),
            ) =>
          result.add(v)
        case LetStep(
              Variable(v, _),
              ReferenceExpression(Access(_, "MapData", _, _)),
            ) =>
          result.add(v)
        case SetStep(
              Access(_, "MapData", _, _),
              ReferenceExpression(Variable(v, _)),
            ) =>
          result.add(v)
        case _ => super.walk(s)
    }.walk(step)
    result.toSet
  }

  def isMapData(ref: Reference, mapDataVars: Set[String]) = ref match
    case Access(_, "MapData", _, _) => true
    case Variable(v, _)             => mapDataVars.contains(v)
    case _                          => false

  def searchLoopVariable(body: Step, index: String): (Reference, String) = {
    var base: Option[Reference] = None
    var varName: Option[String] = None
    new LangUnitWalker {
      override def walk(step: Step): Unit = step match
        case LetStep(
              Variable(x, _),
              ReferenceExpression(
                IndexLookup(b, ReferenceExpression(Variable(i, _))),
              ),
            ) =>
          base = Some(b); varName = Some(x)
        case _ => super.walk(step)
    }.walk(body)
    (base.getOrElse(throw new Error), varName.getOrElse(throw new Error))
  }
}

object SetDataTransformer {
  def apply(targets: List[Algorithm]): List[Algorithm] = {
    val engine = new TransformationRuleEngine("SetData")

    val transformedSteps = targets.map { algo =>
      val setDataVars = searchSetDataVariables(algo.body)
      val isSetDataPredicate = (ref: Reference) => isSetData(ref, setDataVars)

      val rules = List(
        setDataYetsRule,
        setDataHasRule(isSetDataPredicate),
        setDataForEachRule(isSetDataPredicate),
        setDataIndexWhileRule(isSetDataPredicate),
        setDataIteratorWhileRule(isSetDataPredicate),
      ) ++ setDataOperationsRule(isSetDataPredicate)

      val newBody = engine.transformStep(algo.body, rules)

      algo.copy(body = newBody)
    }

    engine.printSummary()

    transformedSteps
  }

  // ================================================================================
  // Transformation Rules
  // ================================================================================

  def setDataYetsRule: TransformationRule = TransformationRule(
    name = "SetData Yet",
    stepPattern = {
      case YetStep(
            YetExpression(
              "Replace the element of _S_.[[SetData]] whose value is _e_ with an element whose value is ~empty~.",
              _,
            ),
          ) =>
        _ =>
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
    },
  )

  def setDataOperationsRule(
    isSetData: Reference => Boolean,
  ): List[TransformationRule] =
    List(
      TransformationRule(
        name = "[OPER] SetData Create",
        stepPattern = {
          case SetStep(ref, ListExpression(ListExpressionForm.EmptyList(_, _)))
              if isSetData(ref) =>
            _ =>
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
            _ =>
              LetStep(
                ref,
                InvokeAbstractOperationExpression(
                  "IN__SetDataCreate",
                  List(),
                  HtmlTag.None,
                ),
              )
        },
      ),
      TransformationRule(
        name = "[OPER] SetData Insert",
        stepPattern = {
          case AppendStep(elem, ref) if isSetData(ref) =>
            _ =>
              PerformStep(
                InvokeAbstractOperationExpression(
                  "IN__SetDataInsert",
                  List(ReferenceExpression(ref), elem),
                  HtmlTag.None,
                ),
              )
        },
      ),
      TransformationRule(
        name = "[OPER] SetData Copy",
        stepPattern = {

          case LetStep(variable, ListCopyExpression(ReferenceExpression(ref)))
              if isSetData(ref) =>
            _ =>
              LetStep(
                variable,
                InvokeAbstractOperationExpression(
                  "IN__SetDataCopy",
                  List(ReferenceExpression(ref)),
                  HtmlTag.None,
                ),
              )
        },
      ),
      TransformationRule(
        name = "[OPER] SetData Has",
        expressionPattern = {
          case InvokeAbstractOperationExpression(
                "SetDataHas",
                List(ReferenceExpression(ref), elem),
                tag,
              ) if isSetData(ref) =>
            _ =>
              InvokeAbstractOperationExpression(
                "IN__SetDataHas",
                List(ReferenceExpression(ref), elem),
                tag,
              )
        },
      ),
      TransformationRule(
        name = "[OPER] SetData Size",
        expressionPattern = {
          case InvokeAbstractOperationExpression(
                "SetDataSize",
                List(ReferenceExpression(ref)),
                tag,
              ) if isSetData(ref) =>
            _ =>
              InvokeAbstractOperationExpression(
                "IN__SetDataSize",
                List(ReferenceExpression(ref)),
                tag,
              )
        },
      ),
    )

  def setDataHasRule(isSetData: Reference => Boolean): TransformationRule =
    TransformationRule(
      name = "SetData Has",
      stepPattern = {
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
                        None,
                        elseConfig,
                      ),
                    ),
                  ),
                ),
              ),
            ) if elem == loopElemL && elem == loopElemR && isSetData(ref) =>
          engine =>
            val renameRule = replaceVariableRule(elem, value)
            val transformedThen =
              engine.transformStep(thenStep, List(renameRule))

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
              transformedThen,
              None,
              elseConfig,
            )
      },
    )

  def setDataForEachRule(isSetData: Reference => Boolean): TransformationRule =
    TransformationRule(
      name = "SetData ForEach",
      stepPattern = {
        case ForEachStep(
              _,
              Variable(elem, _),
              ReferenceExpression(ref),
              true,
              body,
            ) if isSetData(ref) =>
          _ =>
            PerformStep(
              InvokeAbstractOperationExpression(
                "IN__SetDataIterateForEach",
                List(
                  ReferenceExpression(ref),
                  AbstractClosureExpression(List(Variable(elem)), List(), body),
                ),
                HtmlTag.None,
              ),
            )
      },
    )

  def setDataIndexWhileRule(
    isSetData: Reference => Boolean,
  ): TransformationRule = TransformationRule(
    name = "SetData While+Index",
    stepBlockPattern = {
      case StepBlock(steps)
          if containsIndexWhileSequence(steps, isSetData).isDefined =>
        engine =>
          def process(list: List[SubStep]): List[SubStep] = list match {
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
                    loopBody,
                  ),
                ) :: tail
                if lengthInit == lengthCond && indexInit == indexCond && isSetData(
                  ref,
                ) =>
              val (loopBase, loopVar) = searchLoopVariable(loopBody, indexInit)

              val removeIterationRule =
                indexWhileRemoveIndexOperationSubRule(lengthInit, indexInit)
              val cleanIterationBody = engine.transformStep(
                loopBody,
                List(removeIterationRule, replaceSetDataRemoveSubRule("Index")),
              )
              val transformedBody = engine.transformStep(
                cleanIterationBody,
                replaceIndexWhileVariableSubRule(loopBase, loopVar, indexInit),
              )

              val loopWithEarlyReturn = wrapWithEarlyReturn(
                engine,
                transformedBody,
                "IN__SetDataIterateForEach",
                loopBase,
                loopVar,
                "Index",
              )
              loopWithEarlyReturn ++ process(tail)
            case head :: tail =>
              head.copy(step =
                engine.transformStep(
                  head.step,
                  List(setDataIndexWhileRule(isSetData)),
                ),
              ) :: process(tail)
            case Nil => Nil
          }
          StepBlock(process(steps))
    },
  )

  def setDataIteratorWhileRule(
    isSetData: Reference => Boolean,
  ): TransformationRule = TransformationRule(
    name = "SetData While+Iterator",
    stepBlockPattern = {
      case StepBlock(steps) if containsIteratorWhileSequence(steps).isDefined =>
        engine =>
          def process(list: List[SubStep]): List[SubStep] = list match {
            case SubStep(
                  _,
                  LetStep(Variable(nextInit, _), EnumLiteral("not-started")),
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
                    loopBody,
                  ),
                ) :: tail if nextInit == nextCond =>
              val iter = searchIteratorVariable(loopBody)

              val rules =
                List(
                  replaceSetDataHasSubRule,
                  replaceSetDataRemoveSubRule("Iterator"),
                )
              val cleanIterationBody = engine.transformStep(loopBody, rules)
              val transformedBody = engine.transformStep(
                cleanIterationBody,
                List(
                  replaceIteratorWhileVariableSubRule(nextInit),
                ),
              )

              val loopWithEarlyReturn = wrapWithEarlyReturn(
                engine,
                transformedBody,
                "IN__SetDataIterateIterator",
                iter,
                nextInit,
                "Iterator",
              )
              loopWithEarlyReturn ++ process(tail)

            case head :: tail =>
              head.copy(step =
                engine.transformStep(
                  head.step,
                  List(setDataIteratorWhileRule(isSetData)),
                ),
              ) :: process(tail)
            case Nil => Nil
          }
          StepBlock(process(steps))
    },
  )

  // ================================================================================
  // SubRules Definitions
  // ================================================================================

  def replaceVariableRule(from: String, to: String): TransformationRule =
    TransformationRule(
      name = "SetData Has >> Rename Variable",
      referencePattern = {
        case Variable(x, nt) if x == from => _ => Variable(to, nt)
      },
    )

  def replaceSetDataRemoveSubRule(parentName: String): TransformationRule =
    TransformationRule(
      name = s"SetData While+$parentName >> SetData Remove [1]",
      stepBlockPattern = {
        case StepBlock(steps) if steps.exists(isSetDataIndexLet) =>
          engine =>
            def process(list: List[SubStep]): List[SubStep] = list match {
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
                val removalStatementSubRule = TransformationRule(
                  name = s"SetData While+$parentName >> SetData Remove [2]",
                  stepPattern = {
                    case SetStep(
                          IndexLookup(b, ReferenceExpression(i)),
                          EnumLiteral("empty"),
                        ) if b == base && i == index =>
                      _ =>
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
                  },
                )
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
                ) :: tail.map(ss =>
                  ss.copy(step =
                    engine
                      .transformStep(ss.step, List(removalStatementSubRule)),
                  ),
                )
              case head :: tail =>
                head.copy(step =
                  engine.transformStep(
                    head.step,
                    List(replaceSetDataRemoveSubRule(parentName)),
                  ),
                ) :: process(tail)
              case Nil => Nil
            }
            StepBlock(process(steps))
      },
    )

  def replaceSetDataHasSubRule: TransformationRule = TransformationRule(
    name = "SetData While+Index >> SetData Has [1]",
    stepBlockPattern = {
      case StepBlock(steps) if steps.exists(isSetDataIndexLet) =>
        engine =>
          def process(list: List[SubStep]): List[SubStep] = list match {
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
              val hasConditionSubRule = TransformationRule(
                name = "SetData While+Index >> SetData Has [2]",
                conditionPattern = {
                  case IsAreCondition(
                        List(ReferenceExpression(i)),
                        neg,
                        List(EnumLiteral("not-found")),
                      ) if i == index =>
                    _ =>
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
                },
              )
              SubStep(
                d,
                LetStep(
                  index,
                  InvokeAbstractOperationExpression(
                    "SetDataIndex",
                    List(ReferenceExpression(base), ReferenceExpression(elem)),
                    t,
                  ),
                ),
              ) :: tail.map(ss =>
                ss.copy(step =
                  engine.transformStep(ss.step, List(hasConditionSubRule)),
                ),
              )
            case head :: tail =>
              head.copy(step =
                engine.transformStep(head.step, List(replaceSetDataHasSubRule)),
              ) :: process(tail)
            case Nil => Nil
          }
          StepBlock(process(steps))
    },
  )

  def indexWhileRemoveIndexOperationSubRule(
    length: String,
    index: String,
  ): TransformationRule = TransformationRule(
    name = "SetData While+Index >> Index Operations",
    stepBlockPattern = {
      case StepBlock(innerSteps)
          if containsIterationSteps(innerSteps, length, index) =>
        engine =>
          def filterSteps(innerList: List[SubStep]): List[SubStep] =
            innerList match {
              case SubStep(
                    _,
                    LetStep(
                      Variable(x, _),
                      ReferenceExpression(
                        IndexLookup(b, ReferenceExpression(Variable(i, _))),
                      ),
                    ),
                  ) :: innerTail if i == index =>
                filterSteps(innerTail)
              case SubStep(_, SetStep(Variable(lLhs, _), _)) :: innerTail
                  if lLhs == length =>
                filterSteps(innerTail)
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
                  ) :: innerTail if iLhs == index && iRhs == index =>
                filterSteps(innerTail)
              case head :: innerTail =>
                head.copy(step =
                  engine.transformStep(
                    head.step,
                    List(indexWhileRemoveIndexOperationSubRule(length, index)),
                  ),
                ) :: filterSteps(innerTail)
              case Nil => Nil
            }
          StepBlock(filterSteps(innerSteps))
    },
  )

  def replaceIndexWhileVariableSubRule(
    base: Reference,
    varName: String,
    index: String,
  ): List[TransformationRule] =
    List(
      TransformationRule(
        name = "SetData While+Index >> Remove Existence Check",
        stepPattern = {
          case IfStep(
                IsAreCondition(
                  List(ReferenceExpression(Variable(elem1, _))),
                  true,
                  List(EnumLiteral("empty")),
                ),
                thenStep,
                None,
                _,
              ) if varName == elem1 =>
            engine =>
              engine.transformStep(
                thenStep,
                replaceIndexWhileVariableSubRule(base, varName, index),
              )
        },
      ),
      TransformationRule(
        name = "SetData While+Index >> SetData Remove",
        stepBlockPattern = {
          case StepBlock(steps)
              if containsLoopVarRemoveSequence(steps, base, index) =>
            engine =>
              def process(list: List[SubStep]): List[SubStep] = list match {
                case SubStep(
                      _,
                      SetStep(
                        IndexLookup(b, ReferenceExpression(Variable(i, _))),
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
                  ) :: process(tail)
                case head :: tail =>
                  head.copy(step =
                    engine.transformStep(
                      head.step,
                      replaceIndexWhileVariableSubRule(base, varName, index),
                    ),
                  ) :: process(tail)
                case Nil => Nil
              }
              StepBlock(process(steps))
        },
      ),
    )

  def replaceIteratorWhileVariableSubRule(varName: String): TransformationRule =
    TransformationRule(
      name = "SetData While+Iterator >> Replace Variable",
      stepPattern = {
        case IfStep(
              IsAreCondition(
                List(ReferenceExpression(Variable(elem1, _))),
                true,
                List(EnumLiteral("done")),
              ),
              thenStep,
              None,
              _,
            ) if varName == elem1 =>
          engine =>
            engine.transformStep(
              thenStep,
              List(replaceIteratorWhileVariableSubRule(varName)),
            )
      },
      stepBlockPattern = {
        case StepBlock(steps)
            if containsIteratorVarRemoveSequence(steps, varName) =>
          engine =>
            def process(list: List[SubStep]): List[SubStep] = list match {
              case SubStep(_, SetStep(Variable(iLhs, _), _)) :: tail
                  if iLhs == varName =>
                process(tail)
              case head :: tail =>
                head.copy(step =
                  engine.transformStep(
                    head.step,
                    List(replaceIteratorWhileVariableSubRule(varName)),
                  ),
                ) :: process(tail)
              case Nil => Nil
            }
            StepBlock(process(steps))
      },
    )

  def earlyReturnSubRule(parentName: String): TransformationRule =
    TransformationRule(
      name = s"SetData While+$parentName >> Early Return",
      stepPattern = {
        case ReturnStep(expr) =>
          _ =>
            ReturnStep(
              RecordExpression(
                "",
                List(
                  (FieldLiteral("Type"), EnumLiteral("early-return")),
                  (FieldLiteral("Value"), expr),
                ),
                RecordExpressionForm.SyntaxLiteral(None),
              ),
            )
      },
    )

  // ================================================================================
  // Transformation Logic Helpers
  // ================================================================================

  def wrapWithEarlyReturn(
    engine: TransformationRuleEngine,
    body: Step,
    aoName: String,
    iterBase: Reference,
    elementVar: String,
    parentName: String,
  ): List[SubStep] = {
    def searchReturns(body: Step): Set[Expression] = {
      var result = mutable.Set[Expression]()
      new LangUnitWalker {
        override def walk(step: Step): Unit = step match {
          case ReturnStep(expr) => result += expr
          case _                => super.walk(step)
        }
      }.walk(body)
      result.toSet
    }

    val resultVariable = Variable("_result")

    val returnReplacedBody = LetStep(
      resultVariable,
      InvokeAbstractOperationExpression(
        aoName,
        List(
          ReferenceExpression(iterBase),
          AbstractClosureExpression(
            List(Variable(elementVar)),
            List(),
            engine.transformStep(body, List(earlyReturnSubRule(parentName))),
          ),
        ),
        HtmlTag.None,
      ),
    )

    val earlyReturnChecks = searchReturns(body).map { expr =>
      IfStep(
        CompoundCondition(
          BinaryCondition(
            ReferenceExpression(resultVariable),
            BinaryConditionOperator.NEq,
            UndefinedLiteral(),
          ),
          CompoundConditionOperator.And,
          BinaryCondition(
            ReferenceExpression(
              Access(
                resultVariable,
                "Type",
                AccessKind.Field,
                AccessForm.Dot,
              ),
            ),
            BinaryConditionOperator.Eq,
            EnumLiteral("early-return"),
          ),
        ),
        ReturnStep(
          ReferenceExpression(
            Access(
              resultVariable,
              "Value",
              AccessKind.Field,
              AccessForm.Dot,
            ),
          ),
        ),
        None,
        IfStep.ElseConfig(),
      )
    }.toList

    (returnReplacedBody +: earlyReturnChecks).map(SubStep(None, _))
  }

  // ================================================================================
  // Sequence Analyzers for Subrules
  // ================================================================================

  @tailrec
  def containsIndexWhileSequence(
    steps: List[SubStep],
    isSetData: Reference => Boolean,
  ): Option[(String, String, String, String, Reference)] = steps match {
    case SubStep(
          _,
          LetStep(
            Variable(lengthInit, _),
            NumberOfExpression("elements", _, ReferenceExpression(ref), _),
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
            _,
          ),
        ) :: _
        if lengthInit == lengthCond && indexInit == indexCond && isSetData(
          ref,
        ) =>
      Some((lengthInit, indexInit, lengthCond, indexCond, ref))
    case _ :: tail => containsIndexWhileSequence(tail, isSetData)
    case Nil       => None
  }

  @tailrec
  def containsIteratorWhileSequence(
    steps: List[SubStep],
  ): Option[(String, String)] = steps match {
    case SubStep(
          _,
          LetStep(Variable(nextInit, _), EnumLiteral("not-started")),
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
            _,
          ),
        ) :: _ if nextInit == nextCond =>
      Some((nextInit, nextCond))
    case _ :: tail => containsIteratorWhileSequence(tail)
    case Nil       => None
  }

  def isSetDataIndexLet(step: SubStep): Boolean = step match {
    case SubStep(
          _,
          LetStep(_, InvokeAbstractOperationExpression("SetDataIndex", _, _)),
        ) =>
      true
    case _ => false
  }

  @tailrec
  def containsIterationSteps(
    steps: List[SubStep],
    length: String,
    index: String,
  ): Boolean = steps match {
    case SubStep(
          _,
          LetStep(
            Variable(_, _),
            ReferenceExpression(
              IndexLookup(_, ReferenceExpression(Variable(i, _))),
            ),
          ),
        ) :: _ if i == index =>
      true
    case SubStep(_, SetStep(Variable(lLhs, _), _)) :: _ if lLhs == length =>
      true
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
        ) :: _ if iLhs == index && iRhs == index =>
      true
    case _ :: tail => containsIterationSteps(tail, length, index)
    case Nil       => false
  }

  @tailrec
  def containsLoopVarRemoveSequence(
    steps: List[SubStep],
    base: Reference,
    index: String,
  ): Boolean = steps match {
    case SubStep(
          _,
          SetStep(
            IndexLookup(b, ReferenceExpression(Variable(i, _))),
            EnumLiteral("empty"),
          ),
        ) :: _ if b == base && i == index =>
      true
    case _ :: tail => containsLoopVarRemoveSequence(tail, base, index)
    case Nil       => false
  }

  @tailrec
  def containsIteratorVarRemoveSequence(
    steps: List[SubStep],
    varName: String,
  ): Boolean = steps match {
    case SubStep(_, SetStep(Variable(iLhs, _), _)) :: _ if iLhs == varName =>
      true
    case _ :: tail => containsIteratorVarRemoveSequence(tail, varName)
    case Nil       => false
  }

  // ================================================================================
  // Analysis Helpers
  // ================================================================================

  def searchSetDataVariables(step: Step): Set[String] = {
    val result = mutable.Set[String]()
    new LangUnitWalker {
      override def walk(s: Step): Unit = s match
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
        case _ => super.walk(s)
    }.walk(step)
    result.toSet
  }

  def isSetData(ref: Reference, setDataVars: Set[String]) = ref match
    case Access(_, "SetData", _, _) => true
    case Variable(v, _)             => setDataVars.contains(v)
    case _                          => false

  def searchLoopVariable(body: Step, index: String): (Reference, String) = {
    var base: Option[Reference] = None
    var varName: Option[String] = None
    new LangUnitWalker {
      override def walk(step: Step): Unit = step match
        case LetStep(
              Variable(x, _),
              ReferenceExpression(
                IndexLookup(b, ReferenceExpression(Variable(i, _))),
              ),
            ) =>
          base = Some(b); varName = Some(x)
        case _ => super.walk(step)
    }.walk(body)
    (base.getOrElse(throw new Error), varName.getOrElse(throw new Error))
  }

  def searchIteratorVariable(body: Step): Reference = {
    var result: Option[Reference] = None
    new LangUnitWalker {
      override def walk(expr: Expression): Unit = expr match {
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
}

object InternalSlotTransformer {
  def apply(targets: List[Algorithm]): List[Algorithm] = {
    val engine = new TransformationRuleEngine("InternalSlot")

    val optimizedTargets = targets.map { algo =>
      val rules = List(
        replaceInternalSlotGet,
        replaceInternalSlotSet,
        replaceRecordCreate,
      )

      val newBody = engine.transformStep(algo.body, rules)

      algo.copy(body = newBody)
    }

    engine.printSummary()
    optimizedTargets
  }

  def replaceInternalSlotGet: TransformationRule = TransformationRule(
    name = "InternalSlot Get",
    stepPattern = {
      case AppendStep(elem, Access(base, name, kind, form)) =>
        _ =>
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__Append",
              List(
                InvokeAbstractOperationExpression(
                  "IN__InternalSlotGet",
                  List(
                    ReferenceExpression(base),
                    StringLiteral(name, StringLiteralForm.SyntaxLiteral),
                  ),
                  HtmlTag.None,
                ),
                elem,
              ),
              HtmlTag.None,
            ),
          )
    },
    expressionPattern = {
      case ReferenceExpression(Access(base, name, kind, form)) =>
        _ =>
          InvokeAbstractOperationExpression(
            "IN__InternalSlotGet",
            List(
              ReferenceExpression(base),
              StringLiteral(name, StringLiteralForm.SyntaxLiteral),
            ),
            HtmlTag.None,
          )
    },
  )

  def replaceInternalSlotSet: TransformationRule = TransformationRule(
    name = "InternalSlot Set",
    stepPattern = {
      case SetStep(Access(base, name, kind, form), expr) =>
        _ =>
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__InternalSlotSet",
              List(
                ReferenceExpression(base),
                StringLiteral(name, StringLiteralForm.SyntaxLiteral),
                expr,
              ),
              HtmlTag.None,
            ),
          )
    },
  )

  def replaceRecordCreate: TransformationRule = TransformationRule(
    name = "InternalSlot Record Create",
    expressionPattern = {
      case expr: RecordExpression =>
        _ =>
          InvokeAbstractOperationExpression(
            "IN__InternalSlotRecordCreate",
            List(expr),
            HtmlTag.None,
          )
    },
  )
}
