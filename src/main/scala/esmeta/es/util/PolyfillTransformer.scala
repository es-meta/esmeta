package esmeta.es.util

import esmeta.lang.*
import esmeta.spec.Algorithm
import esmeta.lang.util.{UnitWalker => LangUnitWalker, Walker => LangWalker}

import scala.collection.mutable
import scala.annotation.tailrec

import dsl.AstExtensions.*

class DSLPath(dslDir: String) extends OptimizationPath {
  def apply(targets: List[Algorithm]) = {
    println(s"Parsing DSL from $dslDir")

    val isSetDataPredicate: dsl.LangElemPredicate = (elem, context) => {
      elem match {
        case Access(_, "SetData", _, _) => true
        case Variable(v, _) =>
          context.variableTypes.get(v).contains("SetData")
        case _ => false
      }
    }

    val isMapDataPredicate: dsl.LangElemPredicate = (elem, context) => {
      elem match {
        case Access(_, "MapData", _, _) => true
        case Variable(v, _) =>
          context.variableTypes.get(v).contains("MapData")
        case _ => false
      }
    }

    val setDataOperRules: List[dsl.Rule] = List(
      // SetData Remove: Replace $elem in $ref with ~empty~
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
      // [OPER] SetData Create (Set)
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
      // [OPER] SetData Create (Let)
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
      // [OPER] SetData Insert
      dsl.StepRule(
        "[OPER] SetData Insert",
        AppendStep(
          MetaExpression("$elem"),
          Variable("$ref"),
        ),
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
      // [OPER] SetData Copy
      dsl.StepRule(
        "[OPER] SetData Copy",
        LetStep(
          Variable("$var"),
          ListCopyExpression(
            ReferenceExpression(Variable("$ref")),
          ),
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
      // [OPER] SetData Has (expression-level)
      dsl.ExpressionRule(
        "[OPER] SetData Has",
        InvokeAbstractOperationExpression(
          "SetDataHas",
          List(
            ReferenceExpression(Variable("$ref")),
            MetaExpression("$elem"),
          ),
          HtmlTag.None,
        ),
        InvokeAbstractOperationExpression(
          "IN__SetDataHas",
          List(
            ReferenceExpression(Variable("$ref")),
            MetaExpression("$elem"),
          ),
          HtmlTag.None,
        ),
        Map("$ref" -> isSetDataPredicate),
      ),
      // [OPER] SetData Size (expression-level)
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

    // SetData Has: ForEach + If(not empty && SameValue) → If(IN__SetDataHas)
    val setDataHasRule: dsl.Rule = dsl.StepRule(
      "SetData Has",
      // pattern: For each $elem of $ref, do
      //   If $elem is not ~empty~ and SameValue($elem, $value) is true, then
      //     $then
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
      // replace: If IN__SetDataHas($ref, $value) is true, then $then
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
      subrules = List.empty,
      // dynamic sub-rule: rename $elem → $value in the thenStep
      dynamicSubrules = bindings => {
        val from = bindings("$elem").asInstanceOf[Variable]
        val to = bindings("$value").asInstanceOf[Variable]
        List(
          dsl.ReferenceRule(
            "SetData Has >> Rename Variable",
            from,
            to,
          ),
        )
      },
    )

    // SetData ForEach: ForEachStep → IN__SetDataIterateForEach + closure
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

    val mapDataOperRules: List[dsl.Rule] = List(
      // [OPER] MapData Insert
      dsl.StepRule(
        "[OPER] MapData Insert",
        AppendStep(
          MetaExpression("$elem"),
          Variable("$ref"),
        ),
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
      // [OPER] MapData Create
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

    // MapData Has: ForEach Record{Key,Value} + If(Key not empty && SameValue)
    //   → If(IN__MapDataHas == true) + sub-rules for Remove/Set/Get
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
      subrules = List.empty,
      dynamicSubrules = bindings => {
        val elem = bindings("$elem").asInstanceOf[Variable]
        val base = bindings("$base").asInstanceOf[Reference]
        val key = bindings("$key").asInstanceOf[Variable]
        List(
          // MapData Has >> MapData Remove: 2-step sequence
          dsl.StepBlockRule(
            "MapData Has >> MapData Remove",
            List(
              SetStep(
                Access(elem, "Key", AccessKind.Field, AccessForm.Dot),
                EnumLiteral("empty"),
              ),
              SetStep(
                Access(elem, "Value", AccessKind.Field, AccessForm.Dot),
                EnumLiteral("empty"),
              ),
            ),
            List(
              PerformStep(
                InvokeAbstractOperationExpression(
                  "IN__MapDataRemove",
                  List(
                    ReferenceExpression(base),
                    ReferenceExpression(key),
                  ),
                  HtmlTag.None,
                ),
              ),
            ),
          ),
          // MapData Has >> MapData Set
          dsl.StepRule(
            "MapData Has >> MapData Set",
            SetStep(
              Access(elem, "Value", AccessKind.Field, AccessForm.Dot),
              MetaExpression("$expr"),
            ),
            Some(
              PerformStep(
                InvokeAbstractOperationExpression(
                  "IN__MapDataSet",
                  List(
                    ReferenceExpression(base),
                    ReferenceExpression(key),
                    MetaExpression("$expr"),
                  ),
                  HtmlTag.None,
                ),
              ),
            ),
          ),
          // MapData Has >> MapData Get
          dsl.StepRule(
            "MapData Has >> MapData Get",
            ReturnStep(
              ReferenceExpression(
                Access(elem, "Value", AccessKind.Field, AccessForm.Dot),
              ),
            ),
            Some(
              ReturnStep(
                InvokeAbstractOperationExpression(
                  "IN__MapDataGet",
                  List(
                    ReferenceExpression(base),
                    ReferenceExpression(key),
                  ),
                  HtmlTag.None,
                ),
              ),
            ),
          ),
        )
      },
    )

    // MapData ForEach: ForEachStep → IN__MapDataIterateForEach + closure
    //   + sub-rule: MapData Remove (2-step Key/Value empty sequence)
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
      subrules = List.empty,
      dynamicSubrules = bindings => {
        val elem = bindings("$elem").asInstanceOf[Variable]
        val ref = bindings("$ref").asInstanceOf[Reference]
        mapDataRemoveSequenceSubRule("MapData ForEach", elem, ref)
      },
    )

    // Predicate: $loopRef is same as $lengthRef, or $loopRef is a copy of $lengthRef
    val isSameOrCopyOf: dsl.LangElemPredicate = (elem, context) => {
      // This predicate is placed on $loopRef. It checks if $loopRef
      // is a copy of any SetData/MapData reference (validated by Analyzer).
      // The actual same-length check is done in dynamicReplace using ctx.copyOf.
      elem match {
        case Variable(v, _) =>
          context.variableTypes.get(v).contains("SetData") ||
          context.variableTypes.get(v).contains("MapData") ||
          context.copyOf.contains(v)
        case Access(_, "SetData", _, _) => true
        case Access(_, "MapData", _, _) => true
        case _                          => false
      }
    }

    // SetData While+Index: Let length + Let index + Repeat(while) with
    //   Let elem = loopRef[index] inside body → early-return closure
    //   $lengthRef and $loopRef may differ (e.g., loopRef is copy of lengthRef)
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
      predicates = Map(
        "$lengthRef" -> isSetDataPredicate,
        "$loopRef" -> isSameOrCopyOf,
      ),
      dynamicReplace = Some { (bindings, ctx, stats) =>
        val lengthRef = bindings("$lengthRef").asInstanceOf[Reference]
        val loopRef = bindings("$loopRef").asInstanceOf[Reference]
        val length = bindings("$length").asInstanceOf[Variable]
        val index = bindings("$index").asInstanceOf[Variable]
        val elem = bindings("$elem").asInstanceOf[Variable]
        val body = bindings("$body").asInstanceOf[Step]

        // Verify same-length: loopRef == lengthRef, or loopRef is copy of lengthRef
        val isSameLength = loopRef == lengthRef || (loopRef match {
          case Variable(v, _) =>
            ctx.copyOf.get(v).contains(lengthRef)
          case _ => false
        })

        if (!isSameLength) {
          println(
            s"  [WARN] SetData While+Index: $loopRef not verified same-length as $lengthRef, skipping",
          )
          List() // bail out — don't transform
        } else {

          // Use loopRef as the iteration base
          val ref = loopRef

          // Sub-rules for body cleanup
          val removeIndexOpsRules: List[dsl.Rule] = List(
            // Delete: Set $length = ...
            dsl.StepRule(
              "SetData While+Index >> Remove Set Length",
              SetStep(length, MetaExpression("$_")),
              None,
            ),
            // Delete: Set $index = $index + 1
            dsl.StepRule(
              "SetData While+Index >> Remove Index Increment",
              SetStep(
                index,
                BinaryExpression(
                  ReferenceExpression(index),
                  BinaryExpressionOperator.Add,
                  DecimalMathValueLiteral(1),
                ),
              ),
              None,
            ),
          )

          // Where-propagation: SetDataIndex → SetData Remove
          val whereRemoveRule = dsl.WhereRule(
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
            whereBindings => {
              val wBase = whereBindings("$wBase").asInstanceOf[Reference]
              val wIndex = whereBindings("$wIndex").asInstanceOf[Variable]
              val wElem = whereBindings("$wElem").asInstanceOf[Reference]
              List(
                dsl.StepRule(
                  "SetData While+Index >> SetData Remove [2]",
                  SetStep(
                    IndexLookup(wBase, ReferenceExpression(wIndex)),
                    EnumLiteral("empty"),
                  ),
                  Some(
                    PerformStep(
                      InvokeAbstractOperationExpression(
                        "IN__SetDataRemove",
                        List(
                          ReferenceExpression(wBase),
                          ReferenceExpression(wElem),
                        ),
                        HtmlTag.None,
                      ),
                    ),
                  ),
                ),
                dsl.ConditionRule(
                  "SetData While+Index >> SetData Has [2]",
                  IsAreCondition(
                    List(ReferenceExpression(wIndex)),
                    false,
                    List(EnumLiteral("not-found")),
                  ),
                  IsAreCondition(
                    List(
                      InvokeAbstractOperationExpression(
                        "IN__SetDataHas",
                        List(
                          ReferenceExpression(wBase),
                          ReferenceExpression(wElem),
                        ),
                        HtmlTag.None,
                      ),
                    ),
                    false,
                    List(FalseLiteral()),
                  ),
                ),
              )
            },
          )

          // SetData Remove (simple index variant)
          val setDataRemoveIndexRule = dsl.StepRule(
            "SetData While+Index >> SetData Remove (index)",
            SetStep(
              IndexLookup(ref, ReferenceExpression(index)),
              EnumLiteral("empty"),
            ),
            Some(
              PerformStep(
                InvokeAbstractOperationExpression(
                  "IN__SetDataRemove",
                  List(
                    ReferenceExpression(ref),
                    ReferenceExpression(elem),
                  ),
                  HtmlTag.None,
                ),
              ),
            ),
          )

          val allSubrules = removeIndexOpsRules ++
            List(whereRemoveRule, setDataRemoveIndexRule)

          dsl.EarlyReturn.wrap(
            body,
            "IN__SetDataIterateForEach",
            ref,
            elem.name,
            allSubrules,
            ctx,
            stats,
          )
        } // end else (isSameLength)
      },
    )

    // SetData While+Iterator: Let next = ~not-started~ + Repeat(while not done)
    //   with Set next = IteratorStepValue($iter) inside body → early-return closure
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
      dynamicReplace = Some { (bindings, ctx, stats) =>
        val iter = bindings("$iter").asInstanceOf[Reference]
        val next = bindings("$next").asInstanceOf[Variable]
        val body = bindings("$body").asInstanceOf[Step]

        // Where-propagation sub-rules for SetDataIndex
        val whereRemoveRule = dsl.WhereRule(
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
          whereBindings => {
            val wBase = whereBindings("$wBase").asInstanceOf[Reference]
            val wIndex = whereBindings("$wIndex").asInstanceOf[Variable]
            val wElem = whereBindings("$wElem").asInstanceOf[Reference]
            List(
              dsl.StepRule(
                "SetData While+Iterator >> SetData Remove [2]",
                SetStep(
                  IndexLookup(wBase, ReferenceExpression(wIndex)),
                  EnumLiteral("empty"),
                ),
                Some(
                  PerformStep(
                    InvokeAbstractOperationExpression(
                      "IN__SetDataRemove",
                      List(
                        ReferenceExpression(wBase),
                        ReferenceExpression(wElem),
                      ),
                      HtmlTag.None,
                    ),
                  ),
                ),
              ),
              dsl.ConditionRule(
                "SetData While+Iterator >> SetData Has [2]",
                IsAreCondition(
                  List(ReferenceExpression(wIndex)),
                  false,
                  List(EnumLiteral("not-found")),
                ),
                IsAreCondition(
                  List(
                    InvokeAbstractOperationExpression(
                      "IN__SetDataHas",
                      List(
                        ReferenceExpression(wBase),
                        ReferenceExpression(wElem),
                      ),
                      HtmlTag.None,
                    ),
                  ),
                  false,
                  List(FalseLiteral()),
                ),
              ),
            )
          },
        )

        val whereHasRule = dsl.WhereRule(
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
          whereBindings => {
            val wBase = whereBindings("$wBase").asInstanceOf[Reference]
            val wIndex = whereBindings("$wIndex").asInstanceOf[Variable]
            val wElem = whereBindings("$wElem").asInstanceOf[Reference]
            List(
              dsl.ConditionRule(
                "SetData While+Iterator >> SetData Has [2]",
                IsAreCondition(
                  List(ReferenceExpression(wIndex)),
                  false,
                  List(EnumLiteral("not-found")),
                ),
                IsAreCondition(
                  List(
                    InvokeAbstractOperationExpression(
                      "IN__SetDataHas",
                      List(
                        ReferenceExpression(wBase),
                        ReferenceExpression(wElem),
                      ),
                      HtmlTag.None,
                    ),
                  ),
                  false,
                  List(FalseLiteral()),
                ),
              ),
            )
          },
        )

        dsl.EarlyReturn.wrap(
          body,
          "IN__SetDataIterateIterator",
          iter,
          next.name,
          List(whereRemoveRule, whereHasRule),
          ctx,
          stats,
        )
      },
    )

    // MapData While: Let length + Let index + Repeat(while)
    //   with Let elem = ref[index] → IN__MapDataIterateWhile + closure (no early return)
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
      dynamicReplace = Some { (bindings, ctx, stats) =>
        val ref = bindings("$ref").asInstanceOf[Reference]
        val length = bindings("$length").asInstanceOf[Variable]
        val index = bindings("$index").asInstanceOf[Variable]
        val elem = bindings("$elem").asInstanceOf[Variable]
        val body = bindings("$body").asInstanceOf[Step]

        // Sub-rules: remove index ops + MapData Remove sequence
        val removeIndexOpsRules: List[dsl.Rule] = List(
          dsl.StepRule(
            "MapData While >> Remove Set Length",
            SetStep(length, MetaExpression("$_")),
            None,
          ),
          dsl.StepRule(
            "MapData While >> Remove Index Increment",
            SetStep(
              index,
              BinaryExpression(
                ReferenceExpression(index),
                BinaryExpressionOperator.Add,
                DecimalMathValueLiteral(1),
              ),
            ),
            None,
          ),
        )

        val allSubrules = removeIndexOpsRules ++
          mapDataRemoveSequenceSubRule("MapData While", elem, ref)

        val transformedBody = allSubrules.foldLeft(body) { (s, rule) =>
          dsl.Transformer.transformStep(rule, s, ctx, stats)
        }

        List(
          PerformStep(
            InvokeAbstractOperationExpression(
              "IN__MapDataIterateWhile",
              List(
                ReferenceExpression(ref),
                AbstractClosureExpression(
                  List(elem),
                  List(),
                  transformedBody,
                ),
              ),
              HtmlTag.None,
            ),
          ),
        )
      },
    )

    // MapData Size: Let count=0 + ForEach counting → IN__MapDataSize
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
        mapDataHasRule,
        mapDataSizeRule,
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

  /** Shared sub-rule: 2-step Key/Value empty → IN__MapDataRemove */
  private def mapDataRemoveSequenceSubRule(
    parent: String,
    elem: Variable,
    base: Reference,
  ): List[dsl.Rule] = List(
    dsl.StepBlockRule(
      s"$parent >> MapData Remove",
      List(
        SetStep(
          Access(elem, "Key", AccessKind.Field, AccessForm.Dot),
          EnumLiteral("empty"),
        ),
        SetStep(
          Access(elem, "Value", AccessKind.Field, AccessForm.Dot),
          EnumLiteral("empty"),
        ),
      ),
      List(
        PerformStep(
          InvokeAbstractOperationExpression(
            "IN__MapDataRemove",
            List(
              ReferenceExpression(base),
              ReferenceExpression(
                Access(
                  elem,
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
  )

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
}
