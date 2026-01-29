package esmeta.es.util
import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq
import esmeta.lang.PredicateConditionOperator.{Abrupt, Present}
import esmeta.spec.*
import esmeta.ty.{NumberIntTy, NumberTy, ValueTy}

import scala.annotation.tailrec

private type Completion = "normal" | "abrupt"

sealed trait CompletionType
case object NormalCompletion extends CompletionType
case object AbruptCompletion extends CompletionType
case object ReturnCompletion extends CompletionType
case object ParameterCompletion extends CompletionType
case object ResolvedParameterCompletion extends CompletionType
case object UnknownCompletion extends CompletionType

// Combined environment for tracking completion types and handled variables
case class CompletionEnv(
  types: Map[String, CompletionType] = Map.empty,
  handled: Set[String] = Set.empty,
) {
  def withType(name: String, ty: CompletionType): CompletionEnv =
    copy(types = types + (name -> ty))
  def withType(name: String, tyStr: String): CompletionEnv = tyStr match {
    case "normal" => copy(types = types + (name -> NormalCompletion))
    case "abrupt" => copy(types = types + (name -> AbruptCompletion))
    case _        => this
  }
  def withHandled(name: String): CompletionEnv =
    copy(handled = handled + name)
  def isHandled(name: String): Boolean = handled.contains(name)
  def getType(name: String): Option[CompletionType] = types.get(name)
}

object PolyfillInspector {

  def process(algo: Algorithm): Algorithm = {
    val newHead = algo.head match {
      case ao @ AbstractOperationHead(_, _, params, _) =>
        val unwrapParams = params.flatMap {
          case p @ Param(name, Type(ty), paramKind) if ty.isCompletion =>
            List(
              p.copy(
                name = s"${name}_type",
                ty = Type(ValueTy(number = NumberTy.Int)),
              ),
              p.copy(ty = Type(ValueTy.Top)),
            )
          case x => Some(x)
        }
        ao.copy(params = unwrapParams)
      case x => x
    }
    algo.copy(head = newHead)
  }

  def process(algo: Algorithm, step: Step): Step = {
    val paramCompletion = algo.head match {
      case ao @ AbstractOperationHead(_, _, params, _) =>
        params.filter {
          case p @ Param(name, Type(ty), paramKind) => ty.isCompletion
        }
      case x => List()
    }
    val env = paramCompletion.foldLeft(CompletionEnv())((it, item) =>
      it.withType(item.name, ParameterCompletion),
    )
    optimize(step :: Nil, Nil, env).toBlockStep
  }

  private def optimize(
    input: List[Step],
    history: List[Step],
    env: CompletionEnv,
  ): List[Step] = input match {
    case (check @ CompletionCheckPattern(checks)) :: tail =>
      val ifStep = check.asInstanceOf[IfStep]
      val (checkType, targetVar) = checks

      if (env.isHandled(targetVar)) {
        // Variable already handled
        if (
          ifStep.elseStep.isEmpty && env
            .getType(targetVar)
            .contains(NormalCompletion)
        ) { // If the completion is already surrounded by try (not catch!), IfStep can be omitted
          transformStep(
            ifStep.thenStep,
            env.withType(targetVar, checkType),
          ) match {
            case (Some(optimizedThen), newEnv) =>
              optimize(tail, optimizedThen :: history, newEnv)
            case (None, newEnv) => optimize(tail, history, newEnv)
          }
        } else {
          val flagName = s"${targetVar}_is_abrupt"
          val taggedCheck = annotateStep(
            annotateStep(
              annotateStep(check, "USE_FLAG", flagName),
              "TYPE",
              checkType,
            ),
            "TARGET_VAR",
            targetVar,
          )
          transformStep(
            taggedCheck,
            env.withType(targetVar, checkType).withHandled(targetVar),
          ) match {
            case (Some(optimizedCheck), newEnv) =>
              optimize(tail, optimizedCheck :: history, newEnv)
            case (None, newEnv) => optimize(tail, history, newEnv)
          }
        }
      } else {
        val lastModifiedAt =
          history.indexWhere(stmt => modifies(stmt, targetVar))
        if (lastModifiedAt != -1) {
          val (gap, rest) = history.splitAt(lastModifiedAt)
          val (producer, newHistory) = rest.splitAt(1)

          val flagName = s"${targetVar}_is_abrupt"
          val flagDecl = LetStep(Variable(flagName, None), FalseLiteral())

          val isAbruptTerminal =
            checkType == "abrupt" && isTerminal(ifStep.thenStep)

          if (gap.isEmpty) {
            // No gap - immediately wrap with try/catch
            val merged = mergeWithFlag(
              producer,
              check,
              targetVar,
              s"_${targetVar}_err",
              flagName,
              checkType,
              isAbruptTerminal,
              env,
            )
            transformStep(
              merged,
              env
                .withHandled(targetVar)
                .withType(targetVar, UnknownCompletion),
            ) match {
              case (Some(optimizedTryCatch), newEnv) =>
                optimize(
                  tail,
                  if (isAbruptTerminal) optimizedTryCatch :: newHistory
                  else optimizedTryCatch :: flagDecl :: newHistory,
                  newEnv,
                )
              case (None, newEnv) => optimize(tail, newHistory, newEnv)
            }

          } else {
            // Gap exists - wrap producer and use flag
            val wrappedProducer = wrapProducerOnly(
              producer,
              targetVar,
              s"_${targetVar}_err",
              flagName,
            )
            val taggedCheck = annotateStep(
              annotateStep(
                annotateStep(check, "USE_FLAG", flagName),
                "TYPE",
                checkType,
              ),
              "TARGET_VAR",
              targetVar,
            )
            val newEnv = env
              .withHandled(targetVar)
              .withType(targetVar, UnknownCompletion)
            transformStep(
              taggedCheck,
              env.withType(targetVar, checkType),
            ) match {
              case (Some(optimizedCheck), newEnv) =>
                optimize(
                  tail,
                  flagDecl :: wrappedProducer :: gap ::: optimizedCheck :: newHistory,
                  newEnv,
                )
              case (None, newEnv) => ???
            }
          }
        } else {
          /* At this block, the completion in condition must be in a function signature(parameter).
            Then the completion should be unpacked as: ${name}_type, ${name}
            - ${name}_type (Number)           : 0 if normal, 1 if abrupt
            - ${name} (ECMAScript Value): unpacked completion value itself
            Thus, we have to make new IfStep to handle this logic.
           */
          // First only mutate the completion condition
          // TODO Only considering Variable Reference
          val checkTypeLiteral = NumberLiteral(
            if (checkType == "normal") 0 else 1,
          )
          val newCond = rebaseCondition(
            ifStep.cond,
            Map(
              targetVar -> BinaryCondition(
                ReferenceExpression(Variable(s"${targetVar}_type", None)),
                Eq,
                checkTypeLiteral,
              ),
            ),
          )
            .getOrElse(
              throw RuntimeException(
                "Checking completion from parameter cannot be omitted",
              ),
            )

          val isAbruptTerminal = isTerminal(ifStep.thenStep)
          val newThenStep = optimize(
            ifStep.thenStep :: Nil,
            Nil,
            env.withType(targetVar, checkType),
          ).toBlockStep

          val newElseStep = ifStep.elseStep.map(it =>
            optimize(
              it :: Nil,
              Nil,
              env.withType(targetVar, checkType),
            ).toBlockStep,
          )

          val newIfStep = ifStep.copy(
            cond = newCond,
            thenStep = newThenStep,
            elseStep = newElseStep,
          )
          // Propagate completion type (already handled)
          val newEnv =
            if (checkType == "abrupt" && isAbruptTerminal)
              env.withType(targetVar, ResolvedParameterCompletion)
            else env
          optimize(tail, newIfStep :: history, newEnv)
        }
      }

    case head :: tail =>
      val (newStepOpt, newEnv) = transformStep(head, env)
      newStepOpt match {
        case Some(newStep) =>
          val unwrapped = StepMapper.mapExpressions(newStep) { expr =>
            unwrapValueAccess(expr, newEnv)
          }
          optimize(tail, unwrapped :: history, newEnv)
        case None =>
          optimize(tail, history, newEnv)
      }

    case Nil => history.reverse
  }

  @tailrec
  private def transformStep(
    step: Step,
    env: CompletionEnv,
  ): (Option[Step], CompletionEnv) = step match {

    case LetStep(v @ Variable(name, _), expr) =>
      val (newExpr, typeUpdate) = optimizeExpr(expr, env)
      if (!env.getType(name).contains(NormalCompletion))
        (
          Some(LetStep(v, newExpr)),
          typeUpdate.map(t => env.withType(name, t)).getOrElse(env),
        )
      else (Some(LetStep(v, newExpr)), env)

    case SetStep(v @ Variable(name, _), expr) =>
      expr match {
        case ReturnIfAbruptExpression(
              ReferenceExpression(Variable(inner, _)),
              false,
            ) if name == inner =>
          // Remove redundant Set x = ! x shorthand
          (None, env)
        case _ =>
          val (newExpr, typeUpdate) = optimizeExpr(expr, env)
          if (!env.getType(name).contains(NormalCompletion))
            (
              Some(SetStep(v, newExpr)),
              typeUpdate.map(t => env.withType(name, t)).getOrElse(env),
            )
          else (Some(SetStep(v, newExpr)), env)
      }

    /*
    case WrappedTryCatchStep(
          tryStep,
          catchVarRef @ Variable(catchVar, _),
          catchStep,
        ) =>
      val newTry = tryStep
      val catchEnv = env.withType(catchVar, AbruptCompletion)
      val newCatch =
        catchStep.map(c => optimize(c :: Nil, Nil, catchEnv).toBlockStep)
      (Some(WrappedTryCatchStep(newTry, catchVarRef, newCatch)), env)
     */
    case ret @ ReturnStep(
          ReturnIfAbruptExpression(ReferenceExpression(Variable(name, _)), true),
        ) =>
      env.getType(name) match {
        case Some(AbruptCompletion) =>
          (Some(TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))), env)
        case Some(ParameterCompletion) =>
          (
            Some(
              IfStep(
                BinaryCondition(
                  ReferenceExpression(
                    Variable(
                      s"${name}_type",
                      None,
                    ),
                  ),
                  Eq,
                  NumberLiteral(1),
                ),
                TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
                Some(ret),
              ),
            ),
            env.withType(name, ResolvedParameterCompletion),
          )
        case Some(UnknownCompletion) =>
          (
            Some(
              IfStep(
                BinaryCondition(
                  ReferenceExpression(
                    Variable(
                      s"${name}_is_abrupt",
                      None,
                    ),
                  ),
                  Eq,
                  TrueLiteral(),
                ),
                TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
                Some(ret),
              ),
            ),
            env.withType(name, ResolvedParameterCompletion),
          )
        case _ => (Some(ret), env)
      }

    case TaggedStep(taggedInnerStep, tag) =>
      taggedInnerStep match {
        case IfStep(cond, thenStep, elseStep, cfg) =>
          // Get the target variable and check type from tags
          val targetVarOpt = tag.get("TARGET_VAR")
          val checkTypeOpt = tag.get("TYPE")

          (targetVarOpt, checkTypeOpt) match {
            case (Some(targetVar), Some(checkType)) =>
              // Process branches with correct completion type for target variable
              val thenType =
                if (checkType == "abrupt") AbruptCompletion
                else NormalCompletion
              val elseType =
                if (checkType == "abrupt") NormalCompletion
                else AbruptCompletion

              val thenEnv = env.withType(targetVar, thenType)
              val elseEnv = env.withType(targetVar, elseType)

              val newThen = optimize(thenStep :: Nil, Nil, thenEnv).toBlockStep
              val newElse =
                elseStep.map(e => optimize(e :: Nil, Nil, elseEnv).toBlockStep)

              val flagVar = tag.getOrElse("USE_FLAG", s"${targetVar}_is_abrupt")
              rebaseCondition(
                cond,
                Map(
                  targetVar -> BinaryCondition(
                    ReferenceExpression(
                      Variable(flagVar, None),
                    ),
                    Eq,
                    if (checkType == "abrupt") TrueLiteral()
                    else FalseLiteral(),
                  ),
                ),
              ) match {
                case Some(newCond) =>
                  (
                    Some(
                      TaggedStep(IfStep(newCond, newThen, newElse, cfg), tag),
                    ),
                    env,
                  )
                // TODO Can we ignore ElseStep? If not, how can we handle it?
                case None =>
                  (
                    Some(newThen),
                    env,
                  ) // If cond is empty, IfStep can be omitted
              }

            case _ =>
              // No completion check tags - just process normally
              val newThen = optimize(thenStep :: Nil, Nil, env).toBlockStep
              val newElse =
                elseStep.map(e => optimize(e :: Nil, Nil, env).toBlockStep)
              rebaseCondition(
                cond,
                Map(),
              ) match {
                case Some(newCond) =>
                  (
                    Some(
                      TaggedStep(IfStep(newCond, newThen, newElse, cfg), tag),
                    ),
                    env,
                  )
                // TODO Can we ignore ElseStep? If not, how can we handle it?
                case None =>
                  (
                    Some(newThen),
                    env,
                  ) // If cond is empty, IfStep can be omitted
              }
          }
        case _ => transformStep(taggedInnerStep, env)
      }

    case BlockStep(stmts) =>
      (Some(optimize(stmts.steps.map(_.step), Nil, env).toBlockStep), env)

    case IfStep(cond, t, e, cfg) =>
      val newT = optimize(t :: Nil, Nil, env).toBlockStep
      val newE = e.map(b => optimize(b :: Nil, Nil, env).toBlockStep)
      (Some(IfStep(cond, newT, newE, cfg)), env)

    case _ => (Some(step), env)
  }

  private def optimizeExpr(
    expr: Expression,
    env: CompletionEnv,
  ): (Expression, Option[CompletionType]) = expr match {
    case InvokeAbstractOperationExpression("Completion", args, _) =>
      (args.head, Some(UnknownCompletion))
    case InvokeAbstractOperationExpression("NormalCompletion", args, _) =>
      (args.head, Some(NormalCompletion))
    case InvokeAbstractOperationExpression("ThrowCompletion", args, _) =>
      (args.head, Some(AbruptCompletion))
    case InvokeAbstractOperationExpression("AbruptCompletion", args, _) =>
      (args.head, Some(AbruptCompletion))
    case ReferenceExpression(Variable(name, _)) =>
      (expr, env.getType(name))
    case _ => (expr, None)
  }

  private def renameVariable(
    expr: Expression,
    targetVar: String,
    renameTo: String,
  ): Expression = {
    def renameExpr(e: Expression): Expression =
      renameVariable(e, targetVar, renameTo)

    def recurseRef(ref: Reference): Reference = ref match {
      case v @ Variable(name, _) if name == targetVar => v.copy(name = renameTo)
      case v: Variable                                => v
      case a @ Access(base, _, _, _)   => a.copy(base = recurseRef(base))
      case v @ ValueOf(base)           => v.copy(base = recurseRef(base))
      case i @ IntrinsicField(base, _) => i.copy(base = recurseRef(base))
      case i @ IndexLookup(base, index) =>
        i.copy(base = recurseRef(base), index = renameExpr(index))
      case b @ BindingLookup(base, binding) =>
        b.copy(base = recurseRef(base), binding = renameExpr(binding))
      case n @ NonterminalLookup(base, _) => n.copy(base = recurseRef(base))
      case p @ PositionalElement(base, _) => p.copy(base = recurseRef(base))
      case i @ IntrinsicObject(base, e) =>
        i.copy(base = recurseRef(base), expr = renameExpr(e))
      case r: RunningExecutionContext => r
      case r: SecondExecutionContext  => r
      case r: CurrentRealmRecord      => r
      case r: ActiveFunctionObject    => r
      case r: AgentRecord             => r
    }

    expr match {
      case ReferenceExpression(ref) => ReferenceExpression(recurseRef(ref))
      case StringConcatExpression(exprs) =>
        StringConcatExpression(exprs.map(renameExpr))
      case ListConcatExpression(exprs) =>
        ListConcatExpression(exprs.map(renameExpr))
      case ListCopyExpression(e) => ListCopyExpression(renameExpr(e))
      case r @ RecordExpression(_, fields, _) =>
        r.copy(fields = fields.map((f, e) => (f, renameExpr(e))))
      case LengthExpression(e) => LengthExpression(renameExpr(e))
      case s @ SubstringExpression(e, from, to) =>
        s.copy(
          expr = renameExpr(e),
          from = renameExpr(from),
          to = to.map(renameExpr),
        )
      case t @ TrimExpression(e, _, _) => t.copy(expr = renameExpr(e))
      case n @ NumberOfExpression(_, _, e, exclude) =>
        n.copy(expr = renameExpr(e), exclude = exclude.map(renameExpr))
      case i: IntrinsicExpression  => i
      case SourceTextExpression(e) => SourceTextExpression(renameExpr(e))
      case CoveredByExpression(code, rule) =>
        CoveredByExpression(renameExpr(code), renameExpr(rule))
      case GetItemsExpression(nt, e) =>
        GetItemsExpression(renameExpr(nt), renameExpr(e))
      case l @ ListExpression(form) =>
        l.copy(form = form match {
          case ListExpressionForm.LiteralSyntax(entries) =>
            ListExpressionForm.LiteralSyntax(entries.map(renameExpr))
          case ListExpressionForm.SoleElement(entry) =>
            ListExpressionForm.SoleElement(renameExpr(entry))
          case e: ListExpressionForm.EmptyList => e
          case r @ ListExpressionForm.IntRange(from, _, to, _, _) =>
            r.copy(
              from = renameExpr(from).asInstanceOf[CalcExpression],
              to = renameExpr(to).asInstanceOf[CalcExpression],
            )
        })
      case x: XRefExpression => x
      case SoleElementExpression(list) =>
        SoleElementExpression(renameExpr(list))
      case CodeUnitAtExpression(base, index) =>
        CodeUnitAtExpression(renameExpr(base), renameExpr(index))
      case StringExpression(e) => StringExpression(renameExpr(e))
      case y: YetExpression    => y
      case i @ InvokeAbstractOperationExpression(_, args, _) =>
        i.copy(args = args.map(renameExpr))
      case i @ InvokeNumericMethodExpression(_, _, args) =>
        i.copy(args = args.map(renameExpr))
      case i @ InvokeAbstractClosureExpression(ref, args) =>
        i.copy(
          ref = recurseRef(ref).asInstanceOf[Variable],
          args = args.map(renameExpr),
        )
      case i @ InvokeMethodExpression(access, args, _) =>
        i.copy(
          access = recurseRef(access).asInstanceOf[Access],
          args = args.map(renameExpr),
        )
      case i @ InvokeSyntaxDirectedOperationExpression(base, _, args, _, _) =>
        i.copy(base = renameExpr(base), args = args.map(renameExpr))
      case r @ ReturnIfAbruptExpression(e, _) => r.copy(expr = renameExpr(e))
      case MathFuncExpression(op, args) =>
        MathFuncExpression(
          op,
          args.map(e => renameExpr(e).asInstanceOf[CalcExpression]),
        )
      case ExponentiationExpression(base, power) =>
        ExponentiationExpression(
          renameExpr(base).asInstanceOf[CalcExpression],
          renameExpr(power).asInstanceOf[CalcExpression],
        )
      case BinaryExpression(left, op, right) =>
        BinaryExpression(
          renameExpr(left).asInstanceOf[CalcExpression],
          op,
          renameExpr(right).asInstanceOf[CalcExpression],
        )
      case UnaryExpression(op, e) =>
        UnaryExpression(op, renameExpr(e).asInstanceOf[CalcExpression])
      case c @ ConversionExpression(_, e, _) => c.copy(expr = renameExpr(e))
      case ClampExpression(target, lower, upper) =>
        ClampExpression(
          renameExpr(target),
          renameExpr(lower),
          renameExpr(upper),
        )
      case MathOpExpression(op, args) =>
        MathOpExpression(
          op,
          args.map(e => renameExpr(e).asInstanceOf[CalcExpression]),
        )
      case BitwiseExpression(left, op, right) =>
        BitwiseExpression(renameExpr(left), op, renameExpr(right))
      case a @ AbstractClosureExpression(_, _, _) =>
        a // Don't recurse into closure body
      case lit: Literal => lit
    }
  }

  private def unwrapValueAccess(
    expr: Expression,
    env: CompletionEnv,
  ): Expression = {
    def recurse(e: Expression): Expression = unwrapValueAccess(e, env)

    def recurseRef(ref: Reference): Reference = ref match {
      case v: Variable                 => v
      case a @ Access(base, _, _, _)   => a.copy(base = recurseRef(base))
      case v @ ValueOf(base)           => v.copy(base = recurseRef(base))
      case i @ IntrinsicField(base, _) => i.copy(base = recurseRef(base))
      case i @ IndexLookup(base, index) =>
        i.copy(base = recurseRef(base), index = recurse(index))
      case b @ BindingLookup(base, binding) =>
        b.copy(base = recurseRef(base), binding = recurse(binding))
      case n @ NonterminalLookup(base, _) => n.copy(base = recurseRef(base))
      case p @ PositionalElement(base, _) => p.copy(base = recurseRef(base))
      case i @ IntrinsicObject(base, e) =>
        i.copy(base = recurseRef(base), expr = recurse(e))
      case r: RunningExecutionContext => r
      case r: SecondExecutionContext  => r
      case r: CurrentRealmRecord      => r
      case r: ActiveFunctionObject    => r
      case r: AgentRecord             => r
    }

    expr match {
      // Special case: unwrap .[[Value]] access on known completion types
      case ReferenceExpression(Access(Variable(varName, _), "Value", _, _)) =>
        env.getType(varName) match {
          case Some(_) => ReferenceExpression(Variable(varName))
          case None    => expr
        }

      case completionAO @ InvokeAbstractOperationExpression(name, args, _)
          if name.contains("Completion") =>
        if (args.length > 1)
          throw RuntimeException(
            s"Completion AO Call should contain up to one argument:\n\t$completionAO",
          )
        args.head
      // Special case: Call with completion argument unpacking
      case aoExpr @ InvokeAbstractOperationExpression(name, args, _) =>
        // if name == "Call" =>
        val newArgs = args.flatMap {
          case x @ ReferenceExpression(Variable(targetVar, _)) =>
            env.getType(targetVar) match {
              case Some(AbruptCompletion)  => List(NumberLiteral(1), x)
              case Some(NormalCompletion)  => List(NumberLiteral(0), x)
              case Some(ReturnCompletion)  => List(NumberLiteral(2), x)
              case Some(UnknownCompletion) => Some(x)
//                throw RuntimeException(
//                  s"Cannot unpack the completion value safely:\n\t$aoExpr",
//                )
              case _ => Some(x)
            }
          case c @ InvokeAbstractOperationExpression(
                innerCallName,
                innerArgs,
                _,
              ) if innerCallName.contains("Completion") =>
            if (innerArgs.length > 1)
              throw RuntimeException(
                s"Completion AO Call should contain up to one argument:\n\t$c",
              )
            innerCallName match {
              case "NormalCompletion" => List(NumberLiteral(0), innerArgs.head)
              case "ThrowCompletion" | "AbruptCompletion" =>
                List(NumberLiteral(1), innerArgs.head)
              case "ReturnCompletion" => List(NumberLiteral(2), innerArgs.head)
              case "Completion" =>
                throw RuntimeException(
                  s"Cannot unpack the raw completion object: $c",
                )
              case _ => Some(c.copy(args = innerArgs.map(recurse)))
            }
          case x => Some(recurse(x))
        }
        aoExpr.copy(args = newArgs)

      // Recursive cases for all expression types
      case ReferenceExpression(ref) => ReferenceExpression(recurseRef(ref))
      case StringConcatExpression(exprs) =>
        StringConcatExpression(exprs.map(recurse))
      case ListConcatExpression(exprs) =>
        ListConcatExpression(exprs.map(recurse))
      case ListCopyExpression(e) => ListCopyExpression(recurse(e))
      case r @ RecordExpression(_, fields, _) =>
        r.copy(fields = fields.map((f, e) => (f, recurse(e))))
      case LengthExpression(e) => LengthExpression(recurse(e))
      case s @ SubstringExpression(e, from, to) =>
        s.copy(expr = recurse(e), from = recurse(from), to = to.map(recurse))
      case t @ TrimExpression(e, _, _) => t.copy(expr = recurse(e))
      case n @ NumberOfExpression(_, _, e, exclude) =>
        n.copy(expr = recurse(e), exclude = exclude.map(recurse))
      case i: IntrinsicExpression  => i
      case SourceTextExpression(e) => SourceTextExpression(recurse(e))
      case CoveredByExpression(code, rule) =>
        CoveredByExpression(recurse(code), recurse(rule))
      case GetItemsExpression(nt, e) =>
        GetItemsExpression(recurse(nt), recurse(e))
      case l @ ListExpression(form) =>
        l.copy(form = form match {
          case ListExpressionForm.LiteralSyntax(entries) =>
            ListExpressionForm.LiteralSyntax(entries.map(recurse))
          case ListExpressionForm.SoleElement(entry) =>
            ListExpressionForm.SoleElement(recurse(entry))
          case e: ListExpressionForm.EmptyList => e
          case r @ ListExpressionForm.IntRange(from, _, to, _, _) =>
            r.copy(
              from = recurse(from).asInstanceOf[CalcExpression],
              to = recurse(to).asInstanceOf[CalcExpression],
            )
        })
      case x: XRefExpression           => x
      case SoleElementExpression(list) => SoleElementExpression(recurse(list))
      case CodeUnitAtExpression(base, index) =>
        CodeUnitAtExpression(recurse(base), recurse(index))
      case StringExpression(e) => StringExpression(recurse(e))
      case y: YetExpression    => y
      case i @ InvokeAbstractOperationExpression(_, args, _) =>
        i.copy(args = args.map(recurse))
      case i @ InvokeNumericMethodExpression(_, _, args) =>
        i.copy(args = args.map(recurse))
      case i @ InvokeAbstractClosureExpression(ref, args) =>
        i.copy(
          ref = recurseRef(ref).asInstanceOf[Variable],
          args = args.map(recurse),
        )
      case i @ InvokeMethodExpression(access, args, _) =>
        i.copy(
          access = recurseRef(access).asInstanceOf[Access],
          args = args.map(recurse),
        )
      case i @ InvokeSyntaxDirectedOperationExpression(base, _, args, _, _) =>
        i.copy(base = recurse(base), args = args.map(recurse))
      case r @ ReturnIfAbruptExpression(e, _) => r.copy(expr = recurse(e))
      case MathFuncExpression(op, args) =>
        MathFuncExpression(
          op,
          args.map(e => recurse(e).asInstanceOf[CalcExpression]),
        )
      case ExponentiationExpression(base, power) =>
        ExponentiationExpression(
          recurse(base).asInstanceOf[CalcExpression],
          recurse(power).asInstanceOf[CalcExpression],
        )
      case BinaryExpression(left, op, right) =>
        BinaryExpression(
          recurse(left).asInstanceOf[CalcExpression],
          op,
          recurse(right).asInstanceOf[CalcExpression],
        )
      case UnaryExpression(op, e) =>
        UnaryExpression(op, recurse(e).asInstanceOf[CalcExpression])
      case c @ ConversionExpression(_, e, _) => c.copy(expr = recurse(e))
      case ClampExpression(target, lower, upper) =>
        ClampExpression(recurse(target), recurse(lower), recurse(upper))
      case MathOpExpression(op, args) =>
        MathOpExpression(
          op,
          args.map(e => recurse(e).asInstanceOf[CalcExpression]),
        )
      case BitwiseExpression(left, op, right) =>
        BitwiseExpression(recurse(left), op, recurse(right))
      case a @ AbstractClosureExpression(_, _, body) =>
        a.copy(body =
          StepMapper.mapExpressions(body)(recurse),
        ) // Don't recurse into closure body
      case lit: Literal => lit
    }
  }

  private def modifies(stmt: Step, varName: String): Boolean = stmt match {
    case LetStep(n, _)           => n.name == varName
    case SetStep(x: Variable, _) => x.name == varName
    case IfStep(_, t, e, _) =>
      modifies(t, varName) || e.exists(modifies(_, varName))
    case BlockStep(StepBlock(stmts)) =>
      stmts.exists(it => modifies(it.step, varName))
    case _ => false
  }

  private def isTerminal(stmt: Step): Boolean = stmt match {
    case ReturnStep(_) => true
    case ThrowStep(_)  => true
    case BlockStep(StepBlock(steps)) =>
      steps.lastOption.exists(it => isTerminal(it.step))
    case IfStep(_, t, Some(e), _)           => isTerminal(t) && isTerminal(e)
    case WrappedTryCatchStep(t, _, Some(c)) => isTerminal(t) && isTerminal(c)
    case _                                  => false
  }

  private def annotateStep(
    step: Step,
    name: String,
    value: String,
  ): TaggedStep =
    step match {
      case TaggedStep(realStep, existingTag) =>
        TaggedStep(realStep, existingTag + (name -> value))
      case x => TaggedStep(x, Map(name -> value))
    }

  private def mergeWithFlag(
    producer: List[Step],
    ifStep: Step,
    varName: String,
    catchVar: String,
    flagName: String,
    completion: String,
    isAbruptTerminal: Boolean,
    env: CompletionEnv,
  ): WrappedTryCatchStep = {
    val IfStep(_, bodyStep, _, _) = ifStep: @unchecked
    if (completion == "normal") {
      val tryStmts = optimize(
        producer :+ bodyStep,
        Nil,
        env.withHandled(varName).withType(varName, NormalCompletion),
      )
      val catchStmts = List(
        SetStep(
          Variable(varName, None),
          ReferenceExpression(Variable(catchVar, None)),
        ),
        SetStep(Variable(flagName, None), TrueLiteral()),
      )
      WrappedTryCatchStep(
        tryStmts.toBlockStep,
        Variable(catchVar),
        Some(catchStmts.toBlockStep),
      )
    } else {
      val tryStmts = optimize(
        producer,
        Nil,
        env.withHandled(varName).withType(varName, NormalCompletion),
      )
      val catchStmts =
        if (!isAbruptTerminal)
          List(
            SetStep(
              Variable(varName, None),
              ReferenceExpression(Variable(catchVar, None)),
            ),
            SetStep(Variable(flagName, None), TrueLiteral()),
            bodyStep,
          )
        else
          List(
            SetStep(
              Variable(varName, None),
              ReferenceExpression(Variable(catchVar, None)),
            ),
            bodyStep,
          )
      val optimizedCatchStmts = optimize(
        catchStmts,
        Nil,
        env.withHandled(varName).withType(varName, AbruptCompletion),
      )
      WrappedTryCatchStep(
        tryStmts.toBlockStep,
        Variable(catchVar),
        Some(optimizedCatchStmts.toBlockStep),
      )
    }
  }

  private def wrapProducerOnly(
    producer: List[Step],
    varName: String,
    catchVar: String,
    flagName: String,
  ): Step = {
    val catchStmts = List(
      SetStep(
        Variable(varName, None),
        ReferenceExpression(Variable(catchVar, None)),
      ),
      SetStep(Variable(flagName, None), TrueLiteral()),
    )
    WrappedTryCatchStep(
      producer.toBlockStep,
      Variable(catchVar),
      Some(catchStmts.toBlockStep),
    )
  }

  private def rebaseCondition(
    cond: Condition,
    completionCondition: Map[String, Condition],
  ): Option[Condition] = cond match {
    case PredicateCondition(
          ReferenceExpression(Variable(targetVar, _)),
          _,
          op,
        ) =>
      op match {
        case Abrupt | PredicateConditionOperator.Throw |
            PredicateConditionOperator.Normal =>
          completionCondition.get(targetVar)
        case _ => Some(cond)
      }
    case compoundCond @ CompoundCondition(left, _, right) =>
      // TODO: Is it safe to ignore op?
      (
        rebaseCondition(left, completionCondition),
        rebaseCondition(right, completionCondition),
      ) match {
        case (Some(newLeft), Some(newRight)) =>
          Some(compoundCond.copy(left = newLeft, right = newRight))
        case (None, Some(newRight)) => Some(newRight)
        case (Some(newLeft), None)  => Some(newLeft)
        case (None, None)           => None
      }
    case _ => Some(cond)
  }

  extension (l: List[Step])
    def toBlockStep: Step = BlockStep(StepBlock(l.map(SubStep(None, _))))
}

private object CompletionCheckPattern {
  // TODO: Considering cases that has up to 1 completion checker
  def unapply(step: Step): Option[(Completion, String)] = step match {
    case IfStep(cond, thenStep, elseStep, config) => traverseCondition(cond)
    case _                                        => None
  }

  private def traverseCondition(cond: Condition): Option[(Completion, String)] =
    cond match {
      case PredicateCondition(expr, _, op) =>
        import PredicateConditionOperator.*
        op match {
          case Abrupt | Throw => Some(("abrupt", extractVarName(expr)))
          case Normal         => Some(("normal", extractVarName(expr)))
          case _              => None
        }
      case CompoundCondition(left, op, right) =>
        traverseCondition(left).orElse(traverseCondition(right))
      case _ => None
    }

  private def extractVarName(expr: Expression) = expr match {
    case ReferenceExpression(Variable(x, _)) => x
    case err =>
      throw RuntimeException(
        s"Expected Reference Expression for extractVarName, but got '${err.toString}'",
      )
  }
}

object StepMapper {
  def mapExpressions(step: Step)(f: Expression => Expression): Step =
    step match {
      case LetStep(v, expr) =>
        LetStep(v, f(expr))
      case SetStep(ref, expr) =>
        SetStep(mapRef(ref)(f), f(expr))
      case SetAsStep(ref, verb, id) =>
        SetAsStep(mapRef(ref)(f), verb, id)
      case SetEvaluationStateStep(context, func, args) =>
        SetEvaluationStateStep(mapRef(context)(f), func, args.map(f))
      case PerformStep(expr) =>
        PerformStep(f(expr))
      case InvokeShorthandStep(name, args) =>
        InvokeShorthandStep(name, args.map(f))
      case AppendStep(elem, ref) =>
        AppendStep(f(elem), mapRef(ref)(f))
      case PrependStep(elem, ref) =>
        PrependStep(f(elem), mapRef(ref)(f))
      case InsertStep(elem, ref) =>
        InsertStep(f(elem), mapRef(ref)(f))
      case AddStep(elem, ref) =>
        AddStep(f(elem), mapRef(ref)(f))
      case RemoveStep(target, prep, list) =>
        RemoveStep(mapRemoveTarget(target)(f), prep, f(list))
      case PushContextStep(ref) =>
        PushContextStep(mapRef(ref)(f))
      case SuspendStep(variable, remove) =>
        SuspendStep(variable, remove)
      case RemoveContextStep(context, restoreTarget) =>
        RemoveContextStep(
          mapRef(context)(f),
          mapRestoreTarget(restoreTarget)(f),
        )
      case AssertStep(cond) =>
        AssertStep(mapCond(cond)(f))
      case IfStep(cond, thenStep, elseStep, config) =>
        IfStep(
          mapCond(cond)(f),
          mapExpressions(thenStep)(f),
          elseStep.map(mapExpressions(_)(f)),
          config,
        )
      case RepeatStep(cond, body) =>
        RepeatStep(mapLoopCond(cond)(f), mapExpressions(body)(f))
      case ForEachStep(ty, variable, expr, forward, body) =>
        ForEachStep(ty, variable, f(expr), forward, mapExpressions(body)(f))
      case ForEachIntegerStep(
            variable,
            low,
            lowInc,
            high,
            highInc,
            ascending,
            body,
          ) =>
        ForEachIntegerStep(
          variable,
          f(low),
          lowInc,
          f(high),
          highInc,
          ascending,
          mapExpressions(body)(f),
        )
      case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
        ForEachOwnPropertyKeyStep(
          key,
          obj,
          mapCond(cond)(f),
          ascending,
          order,
          mapExpressions(body)(f),
        )
      case ForEachParseNodeStep(variable, expr, body) =>
        ForEachParseNodeStep(variable, f(expr), mapExpressions(body)(f))
      case ReturnStep(expr) =>
        ReturnStep(f(expr))
      case ThrowStep(name) =>
        ThrowStep(name)
      case ResumeStep(
            callerContext,
            argument,
            generatorContext,
            param,
            steps,
          ) =>
        ResumeStep(
          mapRef(callerContext)(f),
          f(argument),
          mapRef(generatorContext)(f),
          param,
          steps.map(mapSubStep(_)(f)),
        )
      case ResumeEvaluationStep(context, argument, param, steps) =>
        ResumeEvaluationStep(
          mapRef(context)(f),
          argument.map(f),
          param,
          steps.map(mapSubStep(_)(f)),
        )
      case ResumeTopContextStep() =>
        ResumeTopContextStep()
      case NoteStep(note) =>
        NoteStep(note)
      case BlockStep(StepBlock(steps)) =>
        BlockStep(StepBlock(steps.map(mapSubStep(_)(f))))
      case YetStep(expr) =>
        YetStep(expr)
      case SetFieldsWithIntrinsicsStep(ref, desc) =>
        SetFieldsWithIntrinsicsStep(mapRef(ref)(f), desc)
      case PerformBlockStep(StepBlock(steps), desc) =>
        PerformBlockStep(StepBlock(steps.map(mapSubStep(_)(f))), desc)
      case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
        WrappedTryCatchStep(
          mapExpressions(tryBlock)(f),
          catchVar,
          catchBlock.map(mapExpressions(_)(f)),
        )
      case TaggedStep(innerStep, tag) =>
        TaggedStep(mapExpressions(innerStep)(f), tag)
    }

  private def mapSubStep(sub: SubStep)(f: Expression => Expression): SubStep =
    sub.copy(step = mapExpressions(sub.step)(f))

  private def mapRef(ref: Reference)(f: Expression => Expression): Reference =
    ref match {
      case v: Variable => v
      case Access(base, name, kind, form) =>
        Access(mapRef(base)(f), name, kind, form)
      case ValueOf(base)              => ValueOf(mapRef(base)(f))
      case IntrinsicField(base, intr) => IntrinsicField(mapRef(base)(f), intr)
      case IndexLookup(base, index)   => IndexLookup(mapRef(base)(f), f(index))
      case BindingLookup(base, binding) =>
        BindingLookup(mapRef(base)(f), f(binding))
      case NonterminalLookup(base, nt) => NonterminalLookup(mapRef(base)(f), nt)
      case PositionalElement(base, isFirst) =>
        PositionalElement(mapRef(base)(f), isFirst)
      case IntrinsicObject(base, expr) =>
        IntrinsicObject(mapRef(base)(f), f(expr))
      case r: RunningExecutionContext => r
      case r: SecondExecutionContext  => r
      case r: CurrentRealmRecord      => r
      case r: ActiveFunctionObject    => r
      case r: AgentRecord             => r
    }

  private def mapCond(cond: Condition)(f: Expression => Expression): Condition =
    cond match {
      case ExpressionCondition(expr) => ExpressionCondition(f(expr))
      case TypeCheckCondition(expr, neg, tys) =>
        TypeCheckCondition(f(expr), neg, tys)
      case HasFieldCondition(ref, neg, field, form, tyOpt) =>
        HasFieldCondition(mapRef(ref)(f), neg, field, form, tyOpt)
      case HasBindingCondition(ref, neg, binding) =>
        HasBindingCondition(mapRef(ref)(f), neg, f(binding))
      case ProductionCondition(nt, lhsName, rhsName) =>
        ProductionCondition(nt, lhsName, rhsName)
      case PredicateCondition(expr, neg, op) =>
        PredicateCondition(f(expr), neg, op)
      case IsAreCondition(left, neg, right) =>
        IsAreCondition(left.map(f), neg, right.map(f))
      case BinaryCondition(left, op, right) =>
        BinaryCondition(f(left), op, f(right))
      case InclusiveIntervalCondition(left, neg, from, to, desc) =>
        InclusiveIntervalCondition(f(left), neg, f(from), f(to), desc)
      case ContainsCondition(list, neg, target) =>
        ContainsCondition(f(list), neg, mapContainsTarget(target)(f))
      case CompoundCondition(left, op, right) =>
        CompoundCondition(mapCond(left)(f), op, mapCond(right)(f))
    }

  private def mapContainsTarget(
    target: ContainsConditionTarget,
  )(f: Expression => Expression): ContainsConditionTarget =
    target match {
      case ContainsConditionTarget.Expr(expr) =>
        ContainsConditionTarget.Expr(f(expr))
      case other => other
    }

  private def mapRemoveTarget(
    target: RemoveStep.Target,
  )(f: Expression => Expression): RemoveStep.Target =
    target match {
      case RemoveStep.Target.First(count) =>
        RemoveStep.Target.First(count.map(f))
      case RemoveStep.Target.Last(count) => RemoveStep.Target.Last(count.map(f))
      case RemoveStep.Target.Element(elem) => RemoveStep.Target.Element(f(elem))
    }

  private def mapRestoreTarget(
    target: RemoveContextStep.RestoreTarget,
  )(f: Expression => Expression): RemoveContextStep.RestoreTarget =
    target match {
      case RemoveContextStep.RestoreTarget.NoRestore =>
        RemoveContextStep.RestoreTarget.NoRestore
      case RemoveContextStep.RestoreTarget.StackTop =>
        RemoveContextStep.RestoreTarget.StackTop
      case RemoveContextStep.RestoreTarget.Context(ref) =>
        RemoveContextStep.RestoreTarget.Context(mapRef(ref)(f))
    }

  private def mapLoopCond(
    cond: RepeatStep.LoopCondition,
  )(f: Expression => Expression): RepeatStep.LoopCondition =
    cond match {
      case RepeatStep.LoopCondition.NoCondition =>
        RepeatStep.LoopCondition.NoCondition
      case RepeatStep.LoopCondition.While(c) =>
        RepeatStep.LoopCondition.While(mapCond(c)(f))
      case RepeatStep.LoopCondition.Until(c) =>
        RepeatStep.LoopCondition.Until(mapCond(c)(f))
    }
}
