package esmeta.es.util

import esmeta.es.*
import esmeta.lang.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker, Walker => LangWalker}
import scala.collection.mutable

/** polyfill generator */
object PolyfillGenerator {
  def apply(spec: Spec): List[Polyfill] = new PolyfillGenerator(spec).result

  val defaultTargets = List(
    // Builtin methods
    "INTRINSICS.Array.",
    "INTRINSICS.String.",
    "INTRINSICS.Map",
    "INTRINSICS.Set",
    // "INTRINSICS.Promise",
    "INTRINSICS.Iterator.",

    // Builtin properties
    ":Array",
    ":String",
    ":Map",
    ":Set",
    // ":Promise",
    ":Iterator",

    // Number/BigInt
    "Number::",
    "BigInt::",
  )

  val ignoreTargets = List(
    "ArrayBuffer",
    "TypedArray",

    // ES3
    "INTRINSICS.Array.prototype.concat",
    "INTRINSICS.Array.prototype.join",
    "INTRINSICS.Array.prototype.pop",
    "INTRINSICS.Array.prototype.push",
    "INTRINSICS.Array.prototype.reverse",
    "INTRINSICS.Array.prototype.shift",
    "INTRINSICS.Array.prototype.slice",
    "INTRINSICS.Array.prototype.sort",
    "INTRINSICS.Array.prototype.splice",
    "INTRINSICS.Array.prototype.toLocaleString",
    "INTRINSICS.Array.prototype.toString",
    "INTRINSICS.Array.prototype.unshift",
    // YET
    "INTRINSICS.Array.prototype.entries",
    "INTRINSICS.Array.prototype.keys",
    "INTRINSICS.Array.prototype.toSorted",
    "INTRINSICS.Array.prototype.values",

    // ES3
    "INTRINSICS.String.prototype.charAt",
    "INTRINSICS.String.prototype.charCodeAt",
    "INTRINSICS.String.prototype.concat",
    "INTRINSICS.String.prototype.indexOf",
    "INTRINSICS.String.prototype.lastIndexOf",
    "INTRINSICS.String.prototype.localeCompare",
    "INTRINSICS.String.prototype.match",
    "INTRINSICS.String.prototype.replace",
    "INTRINSICS.String.prototype.search",
    "INTRINSICS.String.prototype.slice",
    "INTRINSICS.String.prototype.split",
    "INTRINSICS.String.prototype.substring",
    "INTRINSICS.String.prototype.toLocaleLowerCase",
    "INTRINSICS.String.prototype.toLocaleUpperCase",
    "INTRINSICS.String.prototype.toLowerCase",
    "INTRINSICS.String.prototype.toString",
    "INTRINSICS.String.prototype.toUpperCase",
    "INTRINSICS.String.prototype.valueOf",
    // YET
    "INTRINSICS.String.prototype.matchAll",
    "INTRINSICS.String.prototype.normalize",
    "INTRINSICS.String.prototype.repeat",
  )

  val exactIncludeTargets = List(
    "AddEntriesFromIterable",
    "AddValueToKeyedGroup",
    // "ArrayCreate",
    // "ArraySpeciesCreate",
    "Call",
    "CanonicalizeKeyedCollectionKey",
    "CodePointAt",
    "Completion",
    "CompletionValue",
    "Construct",
    "Contains",
    "CreateArrayFromList",
    "CreateAsyncFromSyncIterator",
    "CreateDataProperty",
    "CreateDataPropertyOrThrow",
    "CreateIteratorResultObject",
    "DeletePropertyOrThrow",
    "FindViaPredicate",
    "FlattenIntoArray",
    "Get",
    "GetIterator",
    "GetIteratorDirect",
    "GetIteratorFromMethod",
    "GetMethod",
    // "GetPrototypeFromConstructor",
    "GetSetRecord",
    "GetV",
    "GroupBy",
    "HasProperty",
    "IfAbruptCloseIterator",
    "IsArray",
    "IsCallable",
    "IsConstructor",
    "IsObject",
    "IsRegExp",
    "IsStrictlyEqual",
    "IsStringWellFormedUnicode",
    "IteratorClose",
    "IteratorComplete",
    "IteratorNext",
    "IteratorStep",
    "IteratorStepValue",
    "IteratorValue",
    "LengthOfArrayLike",
    "MakeBasicObject",
    "NormalCompletion",
    "OrdinaryCreateFromConstructor",
    "OrdinaryObjectCreate",
    "Prepend",
    "RequireInternalSlot",
    "RequireObjectCoercible",
    "SameType",
    "SameValue",
    "SameValueNonNumber",
    "SameValueZero",
    "Set",
    "SetDataHas",
    "SetDataIndex",
    "SetDataSize",
    "StringIndexOf",
    "StringLastIndexOf",
    "StringPad",
    "StringPaddingBuiltinsImpl",
    "SubString",
    "ThrowCompletion",
    "ToBoolean",
    "ToIntegerOrInfinity",
    "ToLength",
    "ToNumber",
    "ToObject",
    "ToPropertyKey",
    "ToString",
    "ToUInt32",
    "TrimString",
    "UTF16EncodeCodePoint",
    "UTF16SurrogatePairToCodePoint",
  )
}

/** extensible helper of polyfill generator */
class PolyfillGenerator(spec: Spec) {

  import Polyfill.*, PolyfillGenerator.*

  /** generated polyfills */
  lazy val result: List[Polyfill] = for {
    algo <- spec.algorithms
    if (
      // algo.isBuiltin &&
      defaultTargets.exists(algo.name.contains) &&
      !ignoreTargets.exists(algo.name.contains) ||
      exactIncludeTargets.exists(algo.name.equals)
    )
  } yield compile(PolyfillInspector.process(algo))

  private val IS_PRESENT = "IsPresent"
  private val AO_HEADER = "AO";
  private val INTERNAL_HEADER = "IN";
  private val SHORTHAND_HEADER = "SH";
  private val RESERVED_WORDS = Set("return")
  private val YET_RULES = Map(
    (
      "Replace the element of _S_.[[SetData]] whose value is _e_ with an element whose value is ~empty~.",
      "S[\"SetData\"][_x0] = \"empty\"",
    ),
    (
      "set _fillString_ to the String value consisting solely of the code unit 0x0020 (SPACE).",
      "fillString = \" \"",
    ),
    ("Return the code point _cp_.", "return cp"),
  )

  /** compile an algorithm into a polyfill */
  def compile(originalAlgo: Algorithm): Polyfill =
    // TODO remove this after implementing all steps
    println(originalAlgo)
    println("-" * 80)
    val algo = PolyfillInspector.process(originalAlgo)
    val pb = PolyfillBuilder(spec, algo)

    val name = algo.name
    val params = algo.head.originalParams

    val prelude = pb.newScope({
      val shouldInsertIsStrict =
        algo.head.originalParams.forall(_.kind != ParamKind.Variadic)
      if (shouldInsertIsStrict) {
        pb.addStmt(NormalStmt("\"use strict\";"))
      }

      val shouldInsertIsPresent = hasIsPresentCond(algo.body)
      if (shouldInsertIsPresent) {
        algo.head.originalParams.zipWithIndex.foreach((param, index) => {
          pb.addStmt(
            NormalStmt(
              s"var ${param.name}$IS_PRESENT = arguments.length > $index;",
            ),
          )
        })
      }

      algo.head.originalParams.zipWithIndex
        .foreach((param, index) => {
          if (param.kind == ParamKind.Optional)
            pb.addStmt(
              NormalStmt(
                s"var ${param.name} = arguments.length > $index ? arguments[$index] : undefined;",
              ),
            )
        })
    })

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
      val walker = new LangWalker {
        override def walk(stepBlock: StepBlock): StepBlock =
          def walkSubSteps(steps: List[SubStep]): List[SubStep] =
            steps match
              case SubStep(_, SetStep(Variable(iLhs, _), _)) :: tail
                  if iLhs == varName =>
                walkSubSteps(tail)
              case head :: tail => super.walk(head) :: walkSubSteps(tail)
              case Nil          => Nil
          StepBlock(walkSubSteps(stepBlock.steps))
      }
      walker.walk(body)
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

    def replaceSetDataInsert(body: Step): Step = {
      val setDataVariables = searchSetDataVariables(body)
      new LangWalker {
        override def walk(step: Step): Step = step match
          case AppendStep(elem, Variable(base, _))
              if setDataVariables.contains(base) =>
            PerformStep(
              InvokeAbstractOperationExpression(
                "IN__SetDataInsert",
                List(
                  ReferenceExpression(Variable(base, None)),
                  elem,
                ),
                HtmlTag.None,
              ),
            )
          case _ => super.walk(step)
      }.walk(body)
    }

    def searchSetDataVariables(body: Step): mutable.Set[String] = {
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
          case SetStep(
                Access(_, "SetData", _, _),
                ReferenceExpression(Variable(v, nt)),
              ) =>
            result.add(v)
          case _ => super.walk(step)
      }.walk(body)
      result
    }

    def wrapWithEarlyReturn(
      body: Step,
      aoName: String,
      iterBase: Reference,
      elementVar: String,
    ): Step = {
      searchEarlyReturn(body) match {
        case Some(expr) =>
          val bodyWithEarlyReturnCheck =
            InvokeAbstractOperationExpression(
              aoName,
              List(
                ReferenceExpression(iterBase),
                AbstractClosureExpression(
                  List(Variable(elementVar)),
                  List(),
                  body,
                ),
                expr,
              ),
              HtmlTag.None,
            )
          IfStep(
            IsAreCondition(
              List(bodyWithEarlyReturnCheck),
              false,
              List(expr),
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

    val walker = new LangWalker {
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
                      ReferenceExpression(Access(base, "SetData", _, _)),
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
                if lengthInit == lengthCond && indexInit == indexCond =>
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
                replaceLoopVariable(strippedBody, loopBase, loopVar, indexInit)

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

              val strippedBody = replaceSetDataRemove(body)

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
    }
    val body =
      compileWithScope(
        pb,
        PolyfillInspector.process(
          originalAlgo,
          replaceSetDataInsert(walker.walk(algo.body)),
        ),
      )
    Polyfill(name, params, prelude ++ body)

  def hasIsPresentCond(step: Step): Boolean = {
    var found = false
    val walker = new LangUnitWalker {
      override def walk(cond: Condition): Unit =
        import PredicateConditionOperator.*
        cond match
          case PredicateCondition(_, _, Present) => found = true
          case _                                 =>
    }
    walker.walk(step)
    found
  }

  /** compile with a new scope and convert it into a statement */
  def compileWithScope(pb: PolyfillBuilder, step: Step): Stmt = try {
    pb.newScope(compile(pb, step))
  } catch {
    // TODO remove this catch after implementing all steps
    case e: Throwable =>
      println(pb.currentResult)
      println("-" * 80)
      throw e
  }

  /** compile algorithm steps */
  def compile(
    pb: PolyfillBuilder,
    step: Step,
  ): Unit = step match {
    case LetStep(x, expr) =>
      pb.addStmt(NormalStmt(s"var ${compile(x)} = ${compile(pb, expr)};"))
    case SetStep(x, expr) =>
      pb.addStmt(NormalStmt(s"${compile(pb, x)} = ${compile(pb, expr)};"))
    case SetAsStep(x, verb, id)                   => ???
    case SetEvaluationStateStep(base, func, args) => ???
    case PerformStep(expr) =>
      pb.addStmt(NormalStmt(s"${compile(pb, expr)};"))
    case InvokeShorthandStep(x, a) =>
      pb.addStmt(NormalStmt(s"${SHORTHAND_HEADER}__$x(${compile(pb, a)});"))
    case AppendStep(expr, ref) =>
      pb.addStmt(
        NormalStmt(
          s"${INTERNAL_HEADER}__Append(${compile(pb, ref)}, ${compile(pb, expr)})",
        ),
      )
    case InsertStep(expr, ref) => ???
    case PrependStep(expr, ref) =>
      pb.addStmt(
        NormalStmt(
          s"${INTERNAL_HEADER}__Prepend(${compile(pb, ref)}, ${compile(pb, expr)})",
        ),
      )
    case AddStep(expr, ref)         => ???
    case RemoveStep(t, p, l)        => ???
    case PushContextStep(ref)       => ???
    case SuspendStep(ref, rm)       => ???
    case RemoveContextStep(ctxt, t) => ???
    case AssertStep(cond)           => ()
    case IfStep(cond, thenStep, elseStep, config) =>
      pb.addStmt(
        IfStmt(
          compile(pb, cond),
          compileWithScope(pb, thenStep),
          elseStep.map(compileWithScope(pb, _)),
        ),
      )
    case RepeatStep(cond, body) =>
      import RepeatStep.LoopCondition.*
      val compiledCond = cond match
        case NoCondition => "true"
        case While(cond) => compile(pb, cond)
        case Until(cond) => "!" + compile(pb, cond)
      pb.addStmt(WhileStmt(compiledCond, compileWithScope(pb, body)))
    case ForEachStep(ty, elem, expr, forward, body) =>
      val compiledExpr = compile(pb, expr)
      val index = pb.newTId
      val element = compile(elem)
      val end = s"${compiledExpr}.length"
      val loopHead = NormalStmt(s"var $element = $compiledExpr[$index];")
      val compiledBody = compileWithScope(pb, body)
      pb.addStmt(ForEachStmt(index, end, loopHead ++ compiledBody))
    case ForEachIntegerStep(x, low, lowInc, high, highInc, ascending, body) =>
      val compiledLow = compile(pb, low)
      val compiledHigh = compile(pb, high)
      val compiledBody = compileWithScope(pb, body)
      pb.addStmt(
        ForEachIntStmt(
          x.name,
          compiledLow,
          lowInc,
          compiledHigh,
          highInc,
          ascending,
          compiledBody,
        ),
      )
    case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
      ???
    case ForEachParseNodeStep(x, expr, body) => ???
    case ReturnStep(expr) =>
      pb.addStmt(NormalStmt(s"return ${compile(pb, expr)};"))
    case ThrowStep(name) =>
      pb.addStmt(NormalStmt(s"throw new $name;"))
    case ResumeStep(callerCtxt, arg, genCtxt, param, steps) => ???
    case ResumeEvaluationStep(b, aOpt, pOpt, steps)         => ???
    case ResumeTopContextStep()                             => ???
    case NoteStep(note)                                     => ()
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(pb, substep.step)
    case YetStep(expr) => pb.addStmt(NormalStmt(compile(pb, expr)))
    case SetFieldsWithIntrinsicsStep(ref, desc) => ???
    case PerformBlockStep(b, d)                 => ???
    case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
      pb.addStmt(
        TryCatchStmt(
          compileWithScope(pb, tryBlock),
          compile(pb, catchVar),
          compileWithScope(pb, catchBlock.get),
        ),
      )
    case TaggedStep(innerStep, tag) =>
      innerStep match {
        /*case IfStep(cond, thenStep, elseStep, config) =>
          val flagVar = tag.get("USE_FLAG")
          val compiledCond = compile(pb, cond);
          if (compiledCond.isEmpty) { // If completion-checking is the only condition then `if` should be omitted
            if (flagVar.isDefined)
              pb.addStmt(
                IfStmt(s"${flagVar.get}", compileWithScope(pb, thenStep), None),
              )
            else pb.addStmt(compileWithScope(pb, thenStep))
          } else
            pb.addStmt(
              IfStmt(
                compiledCond,
                compileWithScope(pb, thenStep),
                elseStep.map(compileWithScope(pb, _)),
              ),
            )*/
        case ThrowStep(name) => pb.addStmt(NormalStmt(s"throw $name;"))
        case x               => compile(pb, x)
      }
  }

  /** compile local variable */
  def compile(x: Variable): String =
    if (RESERVED_WORDS.contains(x.name))
      s"${x.name}_var"
    else
      x.name

  /** compile references */
  def compile(pb: PolyfillBuilder, ref: Reference): String = ref match {
    case x: Variable                => compile(x)
    case Access(base, name, _, _)   => s"${compile(pb, base)}[\"$name\"]"
    case ValueOf(base)              => ???
    case IntrinsicField(base, intr) => ???
    case IndexLookup(base, index) =>
      s"${compile(pb, base)}[${compile(pb, index)}]"
    case BindingLookup(base, binding)   => ???
    case NonterminalLookup(base, nt)    => ???
    case PositionalElement(base, true)  => s"${compile(pb, base)}[0]"
    case PositionalElement(base, false) => ???
    case IntrinsicObject(base, expr)    => ???
    case RunningExecutionContext()      => ???
    case SecondExecutionContext()       => ???
    case CurrentRealmRecord()           => ???
    case ActiveFunctionObject()         => ???
    case AgentRecord()                  => ???
  }

  /** compile expressions */
  def compile(pb: PolyfillBuilder, expr: Expression): String = expr match {
    case StringConcatExpression(exprs) =>
      exprs
        .map(expr =>
          val e = compile(pb, expr)
          // todo: handle unicode escape sequences properly
          if (e.startsWith("0x")) s"String.fromCharCode($e)" else e,
        )
        .mkString(" + ")
    case ListConcatExpression(es) =>
      s"[].concat(${es.map(compile(pb, _)).mkString(", ")})"
    case ListCopyExpression(expr) => s"${compile(pb, expr)}.slice()"
    case RecordExpression(rawName, fields, form) =>
      s"{${fields.map((fieldLit, fieldExpr) => s"\"${fieldLit.name}\": ${compile(pb, fieldExpr)}").mkString(", ")}}"
    case LengthExpression(ReferenceExpression(ref)) =>
      s"${compile(pb, ref)}.length"
    case LengthExpression(expr) => ???
    case StringExpression(str)  => s"\"str\""
    case SubstringExpression(expr, from, to) =>
      s"${INTERNAL_HEADER}__SubString(${compile(pb, expr)}, ${compile(pb, from)}, ${compile(pb, to)})"
    case TrimExpression(expr, leading, trailing) =>
      s"${INTERNAL_HEADER}__Trim(${compile(pb, expr)}, $leading, $trailing)"
    case NumberOfExpression(_, _, ReferenceExpression(ref), _) =>
      s"${compile(pb, ref)}.length"
    case NumberOfExpression(_, _, expr, _) => ???
    case IntrinsicExpression(intr) =>
      if (intr.props.isEmpty)
        s"${intr.base}"
      else
        s"${intr.base}.${intr.props.mkString(".")}"
    case SourceTextExpression(expr)      => ???
    case CoveredByExpression(code, rule) => ???
    case GetItemsExpression(nt, expr @ NonterminalLiteral(_, _, _, _)) =>
      ???
    case expr: GetItemsExpression => ???
    case InvokeAbstractOperationExpression(name, args, tag) =>
      s"${AO_HEADER}__$name(${compile(pb, args)})"
    case InvokeNumericMethodExpression(ty, name, args) =>
      s"${ty}__$name(${compile(pb, args)})"
    case InvokeAbstractClosureExpression(ref, args) => ???
    case InvokeMethodExpression(ref, args, tag) =>
      s"${compile(pb, ref)}(${compile(pb, args)})"
    case InvokeSyntaxDirectedOperationExpression(
          base,
          name,
          args,
          prefix,
          tag,
        ) =>
      ???
    case ReturnIfAbruptExpression(expr, _) => compile(pb, expr)
    case ListExpression(form) =>
      import ListExpressionForm.*
      form match
        case LiteralSyntax(entries)         => s"[${compile(pb, entries)}]"
        case SoleElement(entry)             => s"[${compile(pb, entry)}]"
        case EmptyList(isNewUsed, typeDesc) => "[]"
        case IntRange(
              from,
              isFromInclusive,
              to,
              isToInclusive,
              isAscending,
            ) =>
          s"${INTERNAL_HEADER}__IntRange(${compile(pb, from)}, $isFromInclusive, ${compile(pb, to)}, $isToInclusive, $isAscending)"
    case YetExpression(str, block) =>
      YET_RULES.getOrElse(
        str,
        s"throw new Error(\"YET: ${str.replace("\"", "\\\"")}\")",
      )
    case ReferenceExpression(ref)     => compile(pb, ref)
    case MathFuncExpression(op, args) => s"${compile(op)}(${compile(pb, args)})"
    case ConversionExpression(op, expr, form) => compile(pb, expr)
    case ExponentiationExpression(base, power) =>
      s"${INTERNAL_HEADER}__pow(${compile(pb, base)}, ${compile(pb, power)})"
    case BinaryExpression(left, op, right) =>
      s"${compile(pb, left)} ${compile(op)} ${compile(pb, right)}"
    case UnaryExpression(op, expr) => s"${compile(op)}${compile(pb, expr)}"
    case ClampExpression(target, lower, upper) =>
      s"${INTERNAL_HEADER}__clamp(${compile(pb, target)}, ${compile(pb, lower)}, ${compile(pb, upper)})"
    case expr: MathOpExpression =>
      import MathOpExpressionOperator.*
      val MathOpExpression(op, args) = expr
      (op, args) match
        case (Neg, List(e))    => s"-${compile(pb, e)}"
        case (Add, List(l, r)) => s"${compile(pb, l)} + ${compile(pb, r)}"
        case (Mul, List(l, r)) => s"${compile(pb, l)} * ${compile(pb, r)}"
        case (Sub, List(l, r)) => s"${compile(pb, l)} - ${compile(pb, r)}"
        case (Pow, List(l, r)) =>
          s"${INTERNAL_HEADER}__pow(${compile(pb, l)}, ${compile(pb, r)})"
        case _ => ???
    case BitwiseExpression(left, op, right) => ???
    case AbstractClosureExpression(params, captured, body) =>
      s"function(${params.map(compile).mkString(", ")}) ${compileWithScope(pb, body)}"
    case XRefExpression(op, id)      => ???
    case SoleElementExpression(list) => ???
    case CodeUnitAtExpression(base, index) =>
      s"${compile(pb, base)}[\"${compile(pb, index)}\"]"
    case lit: Literal => compile(lit)
  }

  /** compile iterable of expressions */
  def compile(
    pb: PolyfillBuilder,
    iterable: Iterable[Expression],
    sep: String = ", ",
  ): String =
    iterable.map(compile(pb, _)).mkString(sep)

  /** compile mathematical operators */
  // def compile(expr: MathOpExpression): String =
  //   import MathOpExpressionOperator.*
  //   val MathOpExpression(op, args) = expr
  //   (op, args) match
  //     case (Neg, List(e))      => s"-${compile(l)}"
  //     case (Add, List(l, r))   => s"${compile(l)} + ${compile(r)}"
  //     case (Mul, List(l, r))   => l + " and " + r
  //     case (Sub, List(l, r))   => l + " minus " + r
  //     case (Pow, List(l, r))   => l + " to the " + r + " power"
  //     case (Expm1, List(e))    => e
  //     case (Log10, List(e))    => e
  //     case (Log2, List(e))     => e
  //     case (Cos, List(e))      => e
  //     case (Cbrt, List(e))     => e
  //     case (Exp, List(e))      => e
  //     case (Cosh, List(e))     => e
  //     case (Sinh, List(e))     => e
  //     case (Tanh, List(e))     => e
  //     case (Acos, List(e))     => e
  //     case (Acosh, List(e))    => e
  //     case (Asinh, List(e))    => e
  //     case (Atanh, List(e))    => e
  //     case (Asin, List(e))     => e
  //     case (Atan2, List(x, y)) => x + " / " + y
  //     case (Atan, List(e))     => e
  //     case (Log1p, List(e))    => e
  //     case (Log, List(e))      => e
  //     case (Sin, List(e))      => e
  //     case (Sqrt, List(e))     => e
  //     case (Tan, List(e))      => e
  // case _ => raise(s"invalid math operationr: $op with $args")

  /** compile binary operators */
  def compile(op: BinaryExpressionOperator): String =
    import BinaryExpressionOperator.*
    op match {
      case Add => "+"
      case Sub => "-"
      case Mul => "*"
      case Div => "/"
      case Mod => "%"
    }

  /** compile unary operators */
  def compile(op: UnaryExpressionOperator): String = op match
    case UnaryExpressionOperator.Neg => "-"

  /** compile bitwise operations */
  def compile(op: BitwiseExpressionOperator): String = ???

  /** compile mathematical function operators */
  def compile(op: MathFuncExpressionOperator): String =
    import MathFuncExpressionOperator.*
    op match {
      case Max      => s"${INTERNAL_HEADER}__max"
      case Min      => s"${INTERNAL_HEADER}__min"
      case Abs      => s"${INTERNAL_HEADER}__abs"
      case Floor    => s"${INTERNAL_HEADER}__floor"
      case Truncate => s"${INTERNAL_HEADER}__truncate"
    }

  /** compile branch conditions */
  def compile(pb: PolyfillBuilder, cond: Condition): String = cond match {
    case ExpressionCondition(expr) => compile(pb, expr)
    case TypeCheckCondition(expr, neg, tys) =>
      val compiledExpr = compile(pb, expr)
      (if (neg) s"!" else "") + tys
        .map(_.normalizedName.toLowerCase())
        .map(tyStr => if (tyStr == "record[object]") "object" else tyStr)
        .map(tyStr => s"typeof $compiledExpr === \"$tyStr\"")
        .reduce((l, r) => s"($l || $r)")
    case HasFieldCondition(ref, neg, field, form, opTy) =>
      (if (neg) s"!" else "") + s"(${compile(pb, field)} in ${compile(pb, ref)})"
    case HasBindingCondition(ref, neg, binding)    => ???
    case ProductionCondition(nt, lhsName, rhsName) => ???
    case PredicateCondition(expr, neg, op) =>
      import PredicateConditionOperator.*
      op match {
        case Finite =>
          (if (neg) s"!" else "") + s"isFinite(${compile(pb, expr)})"
        case Abrupt      => s"COMP__isAbrupt(${compile(pb, expr)})"
        case Throw       => s"COMP__isThrow(${compile(pb, expr)})"
        case Return      => ???
        case Break       => ???
        case Continue    => ???
        case NeverAbrupt => ???
        case Normal      => s"COMP__isNormal(${compile(pb, expr)})"
        case Duplicated  => ???
        case Present => (if (neg) s"!" else "") + compile(pb, expr) + IS_PRESENT
        case Empty   => ???
        case StrictMode       => ???
        case ArrayIndex       => ???
        case FalseToken       => ???
        case TrueToken        => ???
        case DataProperty     => ???
        case AccessorProperty => ???
        case FullyPopulated   => ???
        case Nonterminal      => ???
      }
    case IsAreCondition(left, neg, right) =>
      val es = for (lexpr <- left) yield {
        val l = compile(pb, lexpr)
        val e = right
          .map(rexpr =>
            rexpr match
              case NumberLiteral(n) if n.isNaN => s"isNaN($l)"
              case _ => s"($l === ${compile(pb, rexpr)})",
          )
          .reduce((l, r) => s"($l || $r)")
        (if (neg) s"!" else "") + e
      }
      es.reduce((l, r) => s"($l && $r)")
    case BinaryCondition(left, op, right) =>
      import BinaryConditionOperator.*
      lazy val l = compile(pb, left)
      lazy val r = compile(pb, right)
      op match {
        case Eq               => s"$l === $r"
        case NEq              => s"$l !== $r"
        case LessThan         => s"$l < $r"
        case LessThanEqual    => s"$l <= $r"
        case GreaterThan      => s"$l > $r"
        case GreaterThanEqual => s"$l >= $r"
        case SameCodeUnits    => ???
      }
    case InclusiveIntervalCondition(left, neg, from, to, _) =>
      val l = compile(pb, left)
      val e = s"($l >= ${compile(pb, from)} && $l <= ${compile(pb, to)})"
      (if (neg) s"!" else "") + e
    case ContainsCondition(list, neg, ContainsConditionTarget.Expr(target)) =>
      val c =
        s"${INTERNAL_HEADER}__Contains(${compile(pb, list)}, ${compile(pb, target)})"
      (if (neg) s"!" else "") + c
    case ContainsCondition(list, neg, _) => ???
    case CompoundCondition(left, op, right) =>
      import CompoundConditionOperator.*
      lazy val l = compile(pb, left)
      lazy val r = compile(pb, right)
      op match
        case And   => s"$l && $r"
        case Or    => s"$l || $r"
        case Imply => ???
  }

  def compile(lit: Literal): String =
    lit match {
      case _: ThisLiteral          => "this"
      case _: ThisParseNodeLiteral => ???
      case _: NewTargetLiteral     => "new.target"
      case HexLiteral(hex, hasCodeUnitDescription, isUnicodePrefix, name) =>
        s"0x${hex.toHexString}"
      case CodeLiteral(code)                                    => s"\"$code\""
      case GrammarSymbolLiteral(name, flags)                    => ???
      case NonterminalLiteral(ordinal, name, flags, hasArticle) => ???
      case EnumLiteral(name)                                    => s"\"$name\""
      case StringLiteral(str, _)                                => s"\"$str\""
      case FieldLiteral(name)                                   => s"\"$name\""
      case SymbolLiteral(sym)                  => s"Symbol.$sym"
      case ProductionLiteral(lhs, rhs)         => ???
      case ErrorObjectLiteral(name)            => name
      case _: PositiveInfinityMathValueLiteral => "Infinity"
      case _: NegativeInfinityMathValueLiteral => "-Infinity"
      case DecimalMathValueLiteral(n)          => s"$n"
      case MathConstantLiteral(pre, name)      => ???
      case NumberLiteral(n)                    => if(n.toInt == n) s"${n.toInt}" else s"$n"
      case BigIntLiteral(n)                    => s"${n}n"
      case _: TrueLiteral                      => "true"
      case _: FalseLiteral                     => "false"
      case _: UndefinedLiteral                 => "undefined"
      case _: NullLiteral                      => "null"
      case _: UndefinedTypeLiteral             => ???
      case _: NullTypeLiteral                  => ???
      case _: BooleanTypeLiteral               => ???
      case _: StringTypeLiteral                => ???
      case _: SymbolTypeLiteral                => ???
      case _: NumberTypeLiteral                => ???
      case _: BigIntTypeLiteral                => ???
      case _: ObjectTypeLiteral                => ???
    }
}
