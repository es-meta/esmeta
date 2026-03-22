package esmeta.es.util

import esmeta.lang.*
import esmeta.spec.Algorithm

import scala.annotation.tailrec

import dsl.AstExtensions.*

class DSLPath(dslDir: String) extends OptimizationPath {
  def apply(targets: List[Algorithm]) = {
    println(s"Parsing DSL from $dslDir")

    val rules: List[dsl.Rule] =
      setDataOperRules ++
      List(
        setDataHasRule,
        setDataForEachRule,
        setDataWhileIndexRule,
        setDataWhileIteratorRule,
      ) ++
      mapDataOperRules ++
      List(
        mapDataSizeRule,
        mapDataHasRule,
        mapDataForEachRule,
        mapDataWhileRule,
      )

    val stats = new dsl.TransformStats()

    val result = targets.map { algo =>
      println(s"[*] Processing ${algo.head.fname}")
      val body = pass(algo.body, rules, stats)
      println("=" * 80)
      println()
      algo.copy(body = body)
    }

    stats.printSummary()
    result
  }

  def pass(
    body: Step,
    rules: List[dsl.Rule],
    stats: dsl.TransformStats,
  ): Step = {
    rules.foldLeft(body) { (curr, rule) =>
      val ctx = dsl.Analyzer.buildContext(curr)
      dsl.Transformer.transformStep(rule, curr, ctx, Some(stats))
    }
  }

  @tailrec
  private def fixpoint(
    body: Step,
    rules: List[dsl.Rule],
    stats: dsl.TransformStats,
  ): Step = {
    val nextBody = pass(body, rules, stats)
    if (nextBody == body) body
    else fixpoint(nextBody, rules, stats)
  }

  // ===========================================================================
  // Predicates
  // ===========================================================================

  val isSetDataPredicate: dsl.LangElemPredicate = (elem, context) =>
    elem match {
      case Access(_, "SetData", _, _) => true
      case Variable(v, _) =>
        context.variableTypes.get(v).contains("SetData")
      case _ => false
    }

  val isMapDataPredicate: dsl.LangElemPredicate = (elem, context) =>
    elem match {
      case Access(_, "MapData", _, _) => true
      case Variable(v, _) =>
        context.variableTypes.get(v).contains("MapData")
      case _ => false
    }

  val isSameOrCopyOf: dsl.LangElemPredicate = (elem, context) =>
    elem match {
      case Variable(v, _) =>
        context.variableTypes.get(v).contains("SetData") ||
        context.variableTypes.get(v).contains("MapData") ||
        context.copyOf.contains(v)
      case Access(_, "SetData", _, _) => true
      case Access(_, "MapData", _, _) => true
      case _                          => false
    }

  // ===========================================================================
  // SetData OPER rules
  // ===========================================================================

  val setDataOperRules: List[dsl.Rule] = List(
    // SetData Remove
    dsl.StepRule(
      "SetData Remove",
      ReplaceStep(
        MetaExpression("$elem"),
        EnumLiteral("empty"),
        Variable("$ref"),
      ),
      Some(
        PerformStep(
          InvokeAbstractOperationExpression(
            "IN__SetDataRemove",
            List(
              ReferenceExpression(Variable("$ref")),
              MetaExpression("$elem"),
            ),
            HtmlTag.None,
          ),
        ),
      ),
      Map("$ref" -> isSetDataPredicate),
    ),
    // SetData Create (Set)
    dsl.StepRule(
      "[OPER] SetData Create (Set)",
      SetStep(
        Variable("$ref"),
        ListExpression(ListExpressionForm.EmptyList(true, None)),
      ),
      Some(
        SetStep(
          Variable("$ref"),
          InvokeAbstractOperationExpression(
            "IN__SetDataCreate",
            List(),
            HtmlTag.None,
          ),
        ),
      ),
      Map("$ref" -> isSetDataPredicate),
    ),
    // SetData Create (Let)
    dsl.StepRule(
      "[OPER] SetData Create (Let)",
      LetStep(
        Variable("$ref"),
        ListExpression(ListExpressionForm.EmptyList(true, None)),
      ),
      Some(
        LetStep(
          Variable("$ref"),
          InvokeAbstractOperationExpression(
            "IN__SetDataCreate",
            List(),
            HtmlTag.None,
          ),
        ),
      ),
      Map("$ref" -> isSetDataPredicate),
    ),
    // SetData Insert
    dsl.StepRule(
      "[OPER] SetData Insert",
      AppendStep(MetaExpression("$elem"), Variable("$ref")),
      Some(
        PerformStep(
          InvokeAbstractOperationExpression(
            "IN__SetDataInsert",
            List(
              ReferenceExpression(Variable("$ref")),
              MetaExpression("$elem"),
            ),
            HtmlTag.None,
          ),
        ),
      ),
      Map("$ref" -> isSetDataPredicate),
    ),
    // SetData Copy
    dsl.StepRule(
      "[OPER] SetData Copy",
      LetStep(
        Variable("$var"),
        ListCopyExpression(ReferenceExpression(Variable("$ref"))),
      ),
      Some(
        LetStep(
          Variable("$var"),
          InvokeAbstractOperationExpression(
            "IN__SetDataCopy",
            List(ReferenceExpression(Variable("$ref"))),
            HtmlTag.None,
          ),
        ),
      ),
      Map("$ref" -> isSetDataPredicate),
    ),
    // SetData Has (expression)
    dsl.ExpressionRule(
      "[OPER] SetData Has",
      InvokeAbstractOperationExpression(
        "SetDataHas",
        List(ReferenceExpression(Variable("$ref")), MetaExpression("$elem")),
        HtmlTag.None,
      ),
      InvokeAbstractOperationExpression(
        "IN__SetDataHas",
        List(ReferenceExpression(Variable("$ref")), MetaExpression("$elem")),
        HtmlTag.None,
      ),
      Map("$ref" -> isSetDataPredicate),
    ),
    // SetData Size (expression)
    dsl.ExpressionRule(
      "[OPER] SetData Size",
      InvokeAbstractOperationExpression(
        "SetDataSize",
        List(ReferenceExpression(Variable("$ref"))),
        HtmlTag.None,
      ),
      InvokeAbstractOperationExpression(
        "IN__SetDataSize",
        List(ReferenceExpression(Variable("$ref"))),
        HtmlTag.None,
      ),
      Map("$ref" -> isSetDataPredicate),
    ),
  )

  // ===========================================================================
  // SetData Has
  // ===========================================================================

  val setDataHasRule: dsl.Rule = dsl.StepRule(
    "SetData Has",
    ForEachStep(
      None,
      Variable("$elem"),
      ReferenceExpression(Variable("$ref")),
      true,
      BlockStep(
        StepBlock(
          List(
            SubStep(
              None,
              IfStep(
                CompoundCondition(
                  IsAreCondition(
                    List(ReferenceExpression(Variable("$elem"))),
                    true,
                    List(EnumLiteral("empty")),
                  ),
                  CompoundConditionOperator.And,
                  IsAreCondition(
                    List(
                      InvokeAbstractOperationExpression(
                        "SameValue",
                        List(
                          ReferenceExpression(Variable("$elem")),
                          ReferenceExpression(Variable("$value")),
                        ),
                        HtmlTag.None,
                      ),
                    ),
                    false,
                    List(TrueLiteral()),
                  ),
                ),
                MetaStep("$then", false),
                None,
                IfStep.ElseConfig(),
              ),
            ),
          ),
        ),
      ),
    ),
    Some(
      IfStep(
        IsAreCondition(
          List(
            InvokeAbstractOperationExpression(
              "IN__SetDataHas",
              List(
                ReferenceExpression(Variable("$ref")),
                ReferenceExpression(Variable("$value")),
              ),
              HtmlTag.None,
            ),
          ),
          false,
          List(TrueLiteral()),
        ),
        MetaStep("$then", false),
        None,
        IfStep.ElseConfig(),
      ),
    ),
    Map("$ref" -> isSetDataPredicate),
    // Sub-rule: rename $elem → $value (uses parent bindings via pre-substitution)
    subrules = List(
      dsl.ReferenceRule(
        "SetData Has >> Rename Variable",
        Variable("$elem"),
        Variable("$value"),
      ),
    ),
  )

  // ===========================================================================
  // SetData ForEach
  // ===========================================================================

  val setDataForEachRule: dsl.Rule = dsl.StepRule(
    "SetData ForEach",
    ForEachStep(
      None,
      Variable("$elem"),
      ReferenceExpression(Variable("$ref")),
      true,
      MetaStep("$body", false),
    ),
    Some(
      PerformStep(
        InvokeAbstractOperationExpression(
          "IN__SetDataIterateForEach",
          List(
            ReferenceExpression(Variable("$ref")),
            AbstractClosureExpression(
              List(Variable("$elem")),
              List(),
              MetaStep("$body", false),
            ),
          ),
          HtmlTag.None,
        ),
      ),
    ),
    Map("$ref" -> isSetDataPredicate),
  )

  // ===========================================================================
  // SetData While+Index
  // ===========================================================================

  val setDataWhileIndexRule: dsl.Rule = dsl.StepBlockRule(
    name = "SetData While+Index",
    patternSteps = List(
      LetStep(
        Variable("$length"),
        NumberOfExpression(
          "elements",
          None,
          ReferenceExpression(Variable("$lengthRef")),
          None,
        ),
      ),
      LetStep(Variable("$index"), DecimalMathValueLiteral(0)),
      RepeatStep(
        RepeatStep.LoopCondition.While(
          BinaryCondition(
            ReferenceExpression(Variable("$index")),
            BinaryConditionOperator.LessThan,
            ReferenceExpression(Variable("$length")),
          ),
        ),
        BlockStep(
          StepBlock(
            List(
              SubStep(
                None,
                LetStep(
                  Variable("$elem"),
                  ReferenceExpression(
                    IndexLookup(
                      Variable("$loopRef"),
                      ReferenceExpression(Variable("$index")),
                    ),
                  ),
                ),
              ),
              SubStep(None, MetaStep("$body", true)),
            ),
          ),
        ),
      ),
    ),
    predicates =
      Map("$lengthRef" -> isSetDataPredicate, "$loopRef" -> isSameOrCopyOf),
    copyCheck = Some(("$loopRef", "$lengthRef")),
    closureConfig = Some(
      dsl.ClosureConfig(
        aoName = "IN__SetDataIterateForEach",
        iterBase = "$loopRef",
        elementVar = "$elem",
        earlyReturn = true,
      ),
    ),
    subrules = List(
      // Delete: Set $length = ...
      dsl.StepRule(
        "SetData While+Index >> Remove Set Length",
        SetStep(Variable("$length"), MetaExpression("$_")),
        None,
      ),
      // Delete: Set $index = $index + 1
      dsl.StepRule(
        "SetData While+Index >> Remove Index Increment",
        SetStep(
          Variable("$index"),
          BinaryExpression(
            ReferenceExpression(Variable("$index")),
            BinaryExpressionOperator.Add,
            DecimalMathValueLiteral(1),
          ),
        ),
        None,
      ),
      // Where-propagation: SetDataIndex → SetData Remove / Has
      dsl.WhereRule(
        "SetData While+Index >> SetData Remove",
        LetStep(
          Variable("$wIndex"),
          InvokeAbstractOperationExpression(
            "SetDataIndex",
            List(
              ReferenceExpression(Variable("$wBase")),
              ReferenceExpression(Variable("$wElem")),
            ),
            HtmlTag.None,
          ),
        ),
        List(
          dsl.StepRule(
            "SetData While+Index >> SetData Remove [2]",
            SetStep(
              IndexLookup(
                Variable("$wBase"),
                ReferenceExpression(Variable("$wIndex")),
              ),
              EnumLiteral("empty"),
            ),
            Some(
              PerformStep(
                InvokeAbstractOperationExpression(
                  "IN__SetDataRemove",
                  List(
                    ReferenceExpression(Variable("$wBase")),
                    ReferenceExpression(Variable("$wElem")),
                  ),
                  HtmlTag.None,
                ),
              ),
            ),
          ),
          dsl.ConditionRule(
            "SetData While+Index >> SetData Has [2]",
            IsAreCondition(
              List(ReferenceExpression(Variable("$wIndex"))),
              false,
              List(EnumLiteral("not-found")),
            ),
            IsAreCondition(
              List(
                InvokeAbstractOperationExpression(
                  "IN__SetDataHas",
                  List(
                    ReferenceExpression(Variable("$wBase")),
                    ReferenceExpression(Variable("$wElem")),
                  ),
                  HtmlTag.None,
                ),
              ),
              false,
              List(FalseLiteral()),
            ),
          ),
        ),
      ),
      // SetData Remove (simple index variant)
      dsl.StepRule(
        "SetData While+Index >> SetData Remove (index)",
        SetStep(
          IndexLookup(
            Variable("$loopRef"),
            ReferenceExpression(Variable("$index")),
          ),
          EnumLiteral("empty"),
        ),
        Some(
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__SetDataRemove",
              List(
                ReferenceExpression(Variable("$loopRef")),
                ReferenceExpression(Variable("$elem")),
              ),
              HtmlTag.None,
            ),
          ),
        ),
      ),
    ),
  )

  // ===========================================================================
  // SetData While+Iterator
  // ===========================================================================

  val setDataWhileIteratorRule: dsl.Rule = dsl.StepBlockRule(
    name = "SetData While+Iterator",
    patternSteps = List(
      LetStep(Variable("$next"), EnumLiteral("not-started")),
      RepeatStep(
        RepeatStep.LoopCondition.While(
          IsAreCondition(
            List(ReferenceExpression(Variable("$next"))),
            true,
            List(EnumLiteral("done")),
          ),
        ),
        BlockStep(
          StepBlock(
            List(
              SubStep(
                None,
                SetStep(
                  Variable("$next"),
                  ReturnIfAbruptExpression(
                    InvokeAbstractOperationExpression(
                      "IteratorStepValue",
                      List(ReferenceExpression(Variable("$iter"))),
                      HtmlTag.None,
                    ),
                    true,
                  ),
                ),
              ),
              SubStep(None, MetaStep("$body", true)),
            ),
          ),
        ),
      ),
    ),
    closureConfig = Some(
      dsl.ClosureConfig(
        aoName = "IN__SetDataIterateIterator",
        iterBase = "$iter",
        elementVar = "$next",
        earlyReturn = true,
      ),
    ),
    subrules = List(
      // Where-propagation: SetDataIndex → SetData Remove / Has
      dsl.WhereRule(
        "SetData While+Iterator >> SetData Remove",
        LetStep(
          Variable("$wIndex"),
          InvokeAbstractOperationExpression(
            "SetDataIndex",
            List(
              ReferenceExpression(Variable("$wBase")),
              ReferenceExpression(Variable("$wElem")),
            ),
            HtmlTag.None,
          ),
        ),
        List(
          dsl.StepRule(
            "SetData While+Iterator >> SetData Remove [2]",
            SetStep(
              IndexLookup(
                Variable("$wBase"),
                ReferenceExpression(Variable("$wIndex")),
              ),
              EnumLiteral("empty"),
            ),
            Some(
              PerformStep(
                InvokeAbstractOperationExpression(
                  "IN__SetDataRemove",
                  List(
                    ReferenceExpression(Variable("$wBase")),
                    ReferenceExpression(Variable("$wElem")),
                  ),
                  HtmlTag.None,
                ),
              ),
            ),
          ),
          dsl.ConditionRule(
            "SetData While+Iterator >> SetData Has [2]",
            IsAreCondition(
              List(ReferenceExpression(Variable("$wIndex"))),
              false,
              List(EnumLiteral("not-found")),
            ),
            IsAreCondition(
              List(
                InvokeAbstractOperationExpression(
                  "IN__SetDataHas",
                  List(
                    ReferenceExpression(Variable("$wBase")),
                    ReferenceExpression(Variable("$wElem")),
                  ),
                  HtmlTag.None,
                ),
              ),
              false,
              List(FalseLiteral()),
            ),
          ),
        ),
      ),
      dsl.WhereRule(
        "SetData While+Iterator >> SetData Has",
        LetStep(
          Variable("$wIndex"),
          InvokeAbstractOperationExpression(
            "SetDataIndex",
            List(
              ReferenceExpression(Variable("$wBase")),
              ReferenceExpression(Variable("$wElem")),
            ),
            HtmlTag.None,
          ),
        ),
        List(
          dsl.ConditionRule(
            "SetData While+Iterator >> SetData Has [2]",
            IsAreCondition(
              List(ReferenceExpression(Variable("$wIndex"))),
              false,
              List(EnumLiteral("not-found")),
            ),
            IsAreCondition(
              List(
                InvokeAbstractOperationExpression(
                  "IN__SetDataHas",
                  List(
                    ReferenceExpression(Variable("$wBase")),
                    ReferenceExpression(Variable("$wElem")),
                  ),
                  HtmlTag.None,
                ),
              ),
              false,
              List(FalseLiteral()),
            ),
          ),
        ),
      ),
    ),
  )

  // ===========================================================================
  // MapData OPER rules
  // ===========================================================================

  val mapDataOperRules: List[dsl.Rule] = List(
    dsl.StepRule(
      "[OPER] MapData Insert",
      AppendStep(MetaExpression("$elem"), Variable("$ref")),
      Some(
        PerformStep(
          InvokeAbstractOperationExpression(
            "IN__MapDataInsert",
            List(
              ReferenceExpression(Variable("$ref")),
              MetaExpression("$elem"),
            ),
            HtmlTag.None,
          ),
        ),
      ),
      Map("$ref" -> isMapDataPredicate),
    ),
    dsl.StepRule(
      "[OPER] MapData Create",
      SetStep(
        Variable("$ref"),
        ListExpression(ListExpressionForm.EmptyList(true, None)),
      ),
      Some(
        SetStep(
          Variable("$ref"),
          InvokeAbstractOperationExpression(
            "IN__MapDataCreate",
            List(),
            HtmlTag.None,
          ),
        ),
      ),
      Map("$ref" -> isMapDataPredicate),
    ),
  )

  // ===========================================================================
  // MapData Has + sub-rules
  // ===========================================================================

  val mapDataHasRule: dsl.Rule = dsl.StepRule(
    "MapData Has",
    ForEachStep(
      None,
      Variable("$elem"),
      ReferenceExpression(Variable("$base")),
      true,
      BlockStep(
        StepBlock(
          List(
            SubStep(
              None,
              IfStep(
                CompoundCondition(
                  IsAreCondition(
                    List(
                      ReferenceExpression(
                        Access(
                          Variable("$elem"),
                          "Key",
                          AccessKind.Field,
                          AccessForm.Dot,
                        ),
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
                            Access(
                              Variable("$elem"),
                              "Key",
                              AccessKind.Field,
                              AccessForm.Dot,
                            ),
                          ),
                          ReferenceExpression(Variable("$key")),
                        ),
                        HtmlTag.None,
                      ),
                    ),
                    false,
                    List(TrueLiteral()),
                  ),
                ),
                MetaStep("$then", false),
                None,
                IfStep.ElseConfig(),
              ),
            ),
          ),
        ),
      ),
    ),
    Some(
      IfStep(
        BinaryCondition(
          InvokeAbstractOperationExpression(
            "IN__MapDataHas",
            List(
              ReferenceExpression(Variable("$base")),
              ReferenceExpression(Variable("$key")),
            ),
            HtmlTag.None,
          ),
          BinaryConditionOperator.Eq,
          TrueLiteral(),
        ),
        MetaStep("$then", false),
        None,
        IfStep.ElseConfig(),
      ),
    ),
    Map("$base" -> isMapDataPredicate),
    subrules = List(
      // >> MapData Remove: 2-step Key/Value empty sequence
      dsl.StepBlockRule(
        "MapData Has >> MapData Remove",
        List(
          SetStep(
            Access(Variable("$elem"), "Key", AccessKind.Field, AccessForm.Dot),
            EnumLiteral("empty"),
          ),
          SetStep(
            Access(
              Variable("$elem"),
              "Value",
              AccessKind.Field,
              AccessForm.Dot,
            ),
            EnumLiteral("empty"),
          ),
        ),
        List(
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__MapDataRemove",
              List(
                ReferenceExpression(Variable("$base")),
                ReferenceExpression(Variable("$key")),
              ),
              HtmlTag.None,
            ),
          ),
        ),
      ),
      // >> MapData Set
      dsl.StepRule(
        "MapData Has >> MapData Set",
        SetStep(
          Access(Variable("$elem"), "Value", AccessKind.Field, AccessForm.Dot),
          MetaExpression("$expr"),
        ),
        Some(
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__MapDataSet",
              List(
                ReferenceExpression(Variable("$base")),
                ReferenceExpression(Variable("$key")),
                MetaExpression("$expr"),
              ),
              HtmlTag.None,
            ),
          ),
        ),
      ),
      // >> MapData Get
      dsl.StepRule(
        "MapData Has >> MapData Get",
        ReturnStep(
          ReferenceExpression(
            Access(Variable("$elem"), "Value", AccessKind.Field, AccessForm.Dot),
          ),
        ),
        Some(
          ReturnStep(
            InvokeAbstractOperationExpression(
              "IN__MapDataGet",
              List(
                ReferenceExpression(Variable("$base")),
                ReferenceExpression(Variable("$key")),
              ),
              HtmlTag.None,
            ),
          ),
        ),
      ),
    ),
  )

  // ===========================================================================
  // MapData ForEach + Remove sub-rule
  // ===========================================================================

  val mapDataForEachRule: dsl.Rule = dsl.StepRule(
    "MapData ForEach",
    ForEachStep(
      None,
      Variable("$elem"),
      ReferenceExpression(Variable("$ref")),
      true,
      MetaStep("$body", false),
    ),
    Some(
      PerformStep(
        InvokeAbstractOperationExpression(
          "IN__MapDataIterateForEach",
          List(
            ReferenceExpression(Variable("$ref")),
            AbstractClosureExpression(
              List(Variable("$elem")),
              List(),
              MetaStep("$body", false),
            ),
          ),
          HtmlTag.None,
        ),
      ),
    ),
    Map("$ref" -> isMapDataPredicate),
    subrules = List(
      dsl.StepBlockRule(
        "MapData ForEach >> MapData Remove",
        List(
          SetStep(
            Access(Variable("$elem"), "Key", AccessKind.Field, AccessForm.Dot),
            EnumLiteral("empty"),
          ),
          SetStep(
            Access(
              Variable("$elem"),
              "Value",
              AccessKind.Field,
              AccessForm.Dot,
            ),
            EnumLiteral("empty"),
          ),
        ),
        List(
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__MapDataRemove",
              List(
                ReferenceExpression(Variable("$ref")),
                ReferenceExpression(
                  Access(
                    Variable("$elem"),
                    "Key",
                    AccessKind.Field,
                    AccessForm.Dot,
                  ),
                ),
              ),
              HtmlTag.None,
            ),
          ),
        ),
      ),
    ),
  )

  // ===========================================================================
  // MapData While
  // ===========================================================================

  val mapDataWhileRule: dsl.Rule = dsl.StepBlockRule(
    name = "MapData While",
    patternSteps = List(
      LetStep(
        Variable("$length"),
        NumberOfExpression(
          "elements",
          None,
          ReferenceExpression(Variable("$ref")),
          None,
        ),
      ),
      LetStep(Variable("$index"), DecimalMathValueLiteral(0)),
      RepeatStep(
        RepeatStep.LoopCondition.While(
          BinaryCondition(
            ReferenceExpression(Variable("$index")),
            BinaryConditionOperator.LessThan,
            ReferenceExpression(Variable("$length")),
          ),
        ),
        BlockStep(
          StepBlock(
            List(
              SubStep(
                None,
                LetStep(
                  Variable("$elem"),
                  ReferenceExpression(
                    IndexLookup(
                      Variable("$ref"),
                      ReferenceExpression(Variable("$index")),
                    ),
                  ),
                ),
              ),
              SubStep(None, MetaStep("$body", true)),
            ),
          ),
        ),
      ),
    ),
    predicates = Map("$ref" -> isMapDataPredicate),
    closureConfig = Some(
      dsl.ClosureConfig(
        aoName = "IN__MapDataIterateWhile",
        iterBase = "$ref",
        elementVar = "$elem",
      ),
    ),
    subrules = List(
      dsl.StepRule(
        "MapData While >> Remove Set Length",
        SetStep(Variable("$length"), MetaExpression("$_")),
        None,
      ),
      dsl.StepRule(
        "MapData While >> Remove Index Increment",
        SetStep(
          Variable("$index"),
          BinaryExpression(
            ReferenceExpression(Variable("$index")),
            BinaryExpressionOperator.Add,
            DecimalMathValueLiteral(1),
          ),
        ),
        None,
      ),
      dsl.StepBlockRule(
        "MapData While >> MapData Remove",
        List(
          SetStep(
            Access(Variable("$elem"), "Key", AccessKind.Field, AccessForm.Dot),
            EnumLiteral("empty"),
          ),
          SetStep(
            Access(
              Variable("$elem"),
              "Value",
              AccessKind.Field,
              AccessForm.Dot,
            ),
            EnumLiteral("empty"),
          ),
        ),
        List(
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__MapDataRemove",
              List(
                ReferenceExpression(Variable("$ref")),
                ReferenceExpression(
                  Access(
                    Variable("$elem"),
                    "Key",
                    AccessKind.Field,
                    AccessForm.Dot,
                  ),
                ),
              ),
              HtmlTag.None,
            ),
          ),
        ),
      ),
    ),
  )

  // ===========================================================================
  // MapData Size
  // ===========================================================================

  val mapDataSizeRule: dsl.Rule = dsl.StepBlockRule(
    name = "MapData Size",
    patternSteps = List(
      LetStep(Variable("$count"), DecimalMathValueLiteral(0)),
      ForEachStep(
        None,
        Variable("$elem"),
        ReferenceExpression(Variable("$ref")),
        true,
        BlockStep(
          StepBlock(
            List(
              SubStep(
                None,
                IfStep(
                  IsAreCondition(
                    List(
                      ReferenceExpression(
                        Access(
                          Variable("$elem"),
                          "Key",
                          AccessKind.Field,
                          AccessForm.Dot,
                        ),
                      ),
                    ),
                    true,
                    List(EnumLiteral("empty")),
                  ),
                  BlockStep(
                    StepBlock(
                      List(
                        SubStep(
                          None,
                          SetStep(
                            Variable("$count"),
                            BinaryExpression(
                              ReferenceExpression(Variable("$count")),
                              BinaryExpressionOperator.Add,
                              DecimalMathValueLiteral(1),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                  None,
                  IfStep.ElseConfig(),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
    replace = List(
      LetStep(
        Variable("$count"),
        InvokeAbstractOperationExpression(
          "IN__MapDataSize",
          List(ReferenceExpression(Variable("$ref"))),
          HtmlTag.None,
        ),
      ),
    ),
    predicates = Map("$ref" -> isMapDataPredicate),
  )
}
