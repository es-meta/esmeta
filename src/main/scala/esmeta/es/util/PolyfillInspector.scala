package esmeta.es.util

import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq
import esmeta.lang.IfStep.ElseConfig
import esmeta.lang.util.{UnitWalker as LangUnitWalker, Walker as LangWalker}
import esmeta.spec.*
import esmeta.util.BaseUtils.*
import esmeta.ty.{NumberTy, ValueTy}
import org.jsoup.nodes.Element
import scala.collection.mutable
import CompletionType.*, PredicateConditionOperator.*
import esmeta.es.util.dsl.PredicateExpr.matches

enum CompletionType {
  case MayCompletion
  case MayNormal
  case MayAbrupt
  case NotCompletion

  def toTag: String = this match
    case MayNormal => "normal"
    case MayAbrupt => "abrupt"
    case other     => raise(s"Unexpected completion type in tag: $other")
  def join(that: CompletionType): CompletionType = (this, that) match {
    case _ if this == that  => this
    case (NotCompletion, _) => that
    case (_, NotCompletion) => this
    case _                  => MayCompletion
  }
}
object CompletionType {
  def fromTag(s: String): CompletionType = s match {
    case "normal" => MayNormal
    case "abrupt" => MayAbrupt
    case other    => raise(s"Unknown completion tag: $other")
  }
}

case class TypeEnv(map: Map[String, CompletionType] = Map.empty) {
  def +(pair: (String, CompletionType)): TypeEnv = copy(map = map + pair)
  def -(name: String): TypeEnv = copy(map = map - name)
  def apply(name: String): CompletionType = map.getOrElse(name, NotCompletion)
  def ++(that: TypeEnv): TypeEnv = TypeEnv(
    (this.map.keySet ++ that.map.keySet).toList.map { key =>
      key -> (this(key) join that(key))
    }.toMap,
  )
}

case class Config(
  env: TypeEnv = TypeEnv(),
  steps: Vector[Step] = Vector.empty,
) {
  def clear: Config = copy(steps = Vector.empty)
  def apply(env: TypeEnv): Config = copy(env = env)
  def apply(name: String): CompletionType = env(name)
  def +(pair: (String, CompletionType)): Config = copy(env = env + pair)
  def :+(step: Step): Config = copy(steps = steps :+ unwrap(step))
  def ++(steps: Vector[Step]): Config = copy(steps = this.steps ++ steps)
  def unwrap(step: Step): Step = ValueAccessUnwrapper(env).walk(step)
}

class PolyfillInspector(algo: Algorithm, algos: List[Algorithm]) {
  val head = algo.head
  val body = algo.body

  def transformHead: Head = head match {
    case ao @ AbstractOperationHead(_, _, params, _) =>
      val unwrapParams = params.flatMap {
        case p @ Param(name, Type(ty), paramKind) if ty.isCompletion =>
          List(
            p.copy(name = s"${name}_flag", ty = Type(ValueTy.Top)),
            p.copy(ty = Type(ValueTy.Top)),
          )
        case x => Some(x)
      }
      ao.copy(params = unwrapParams)
    case x => x
  }

  def transformBody: Step = {
    val env = TypeEnv((for {
      param <- head.originalParams
      if param.ty.ty.isCompletion
    } yield param.name -> MayCompletion).toMap)
    transform(body, Config(env)).steps.toBlockStep
  }

  def transform(step: Step, config: Config): Config = step match {
    case InvokeShorthandStep(name, args) =>
      val targetAlgo = algos.find(_.name == name)
      if (targetAlgo.isEmpty) config :+ step
      else {
        val targetStep = targetAlgo.get.body
        val targetParameters = targetAlgo.get.head.originalParams.map(_.name)
        val inlinedStep = (targetParameters zip args).foldLeft(targetStep) {
          (step, paramToArg) =>
            ParameterInlineWalker(paramToArg._1, paramToArg._2).walk(step)
        }
        transform(inlinedStep, config.clear)
      }
    case LetStep(
          Variable(x, _, _, _),
          XRefExpression(XRefExpressionOperator.Algo, id),
        ) =>
      val targetFunction = algos.find(_.name.endsWith(id))
      targetFunction.fold(config :+ step) { func =>
        val extractedHead = func.head.asInstanceOf[BuiltinHead]
        val extractedBody = func.body
        val optimizedClosureBody = transform(
          extractedBody,
          Config(),
        ).steps.toBlockStep
        val params = extractedHead.params
        val closureExpression = AbstractClosureExpression(
          params.map(it => Variable(it.name, Some("xref_inlined"))),
          Nil,
          optimizedClosureBody,
        )
        config :+ LetStep(Variable(x), closureExpression)
      }
    case LetStep(Variable(x, _, _, _), expr) =>
      val (newExpr, typeUpdate) = transform(expr, config)
      wrap(config, x, newExpr, typeUpdate, isDecl = true) + (x -> typeUpdate)
    case SetStep(Variable(x, _, _, _), expr) =>
      val (newExpr, typeUpdate) = transform(expr, config)
      wrap(config, x, newExpr, typeUpdate, isDecl = false) + (x -> typeUpdate)
    case (check @ CompletionCheckPattern(checks)) =>
      val ifStep = check.asInstanceOf[IfStep]
      val (checkType, targetVar) = checks
      val canOmit =
        ifStep.elseStep.isEmpty && config(targetVar) == MayNormal
      if (canOmit) {
        transform(ifStep.thenStep, config + (targetVar -> checkType))
      } else {
        val flagName = s"${targetVar}_flag"
        val taggedCheck = annotateStep(
          annotateStep(
            annotateStep(check, "USE_FLAG", flagName),
            "TYPE",
            checkType.toTag,
          ),
          "TARGET_VAR",
          targetVar,
        )
        transform(taggedCheck, config + (targetVar -> checkType))
      }
    case ReturnStep(
          InvokeAbstractOperationExpression(
            name,
            ReferenceExpression(Variable(varName, _, _, _)) :: Nil,
            _,
          ),
        ) if name == "ThrowCompletion" =>
      config :+ TaggedStep(ThrowStep(varName), Map("reason" -> "abrupt"))
    case ReturnStep(ReferenceExpression(Variable(name, _, _, _)))
        if config(name) == MayAbrupt =>
      config :+ TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))
    // return ? x — ShorthandInliningRule only covers `? x` as a standalone step;
    // `return ? x` is ReturnStep(ReturnIfAbruptExpression(...)) and needs explicit handling.
    case ReturnStep(
          ReturnIfAbruptExpression(
            ReferenceExpression(Variable(name, _, _, _)),
            true,
          ),
        ) =>
      config :+ IfStep(
        BinaryCondition(
          ReferenceExpression(Variable(s"${name}_flag", None)),
          Eq,
          EnumLiteral("abrupt"),
        ),
        TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
        Some(ReturnStep(ReferenceExpression(Variable(name, None)))),
      )
    case ret @ ReturnStep(ReferenceExpression(Variable(name, _, _, _)))
        if config(name) != NotCompletion =>
      config :+ IfStep(
        BinaryCondition(
          ReferenceExpression(Variable(s"${name}_flag", None)),
          Eq,
          EnumLiteral("abrupt"),
        ),
        TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
        Some(ret),
      )
    case IfStep(cond, t, e, cfg) =>
      val Config(thenEnv, thenSteps) = transform(t, config.clear)
      val (elseResult, elseEnv) = e match {
        case Some(b) =>
          val Config(eEnv, steps) = transform(b, config.clear)
          if (steps.isEmpty) (None, eEnv)
          else (Some(steps.toBlockStep), eEnv)
        case None => (None, config.env)
      }
      val mergedEnv = thenEnv ++ elseEnv
      config(mergedEnv) :+ IfStep(cond, thenSteps.toBlockStep, elseResult, cfg)
    case TaggedStep(taggedInnerStep, tag) =>
      taggedInnerStep match {
        case IfStep(cond, thenStep, elseStep, cfg) =>
          val targetVarOpt = tag.get("TARGET_VAR")
          val checkTypeOpt = tag.get("TYPE").map(CompletionType.fromTag)

          (targetVarOpt, checkTypeOpt) match {
            case (Some(targetVar), Some(checkType)) =>
              handleTaggedCompletion(
                cond,
                thenStep,
                elseStep,
                cfg,
                tag,
                targetVar,
                checkType,
                config,
              )
            case _ =>
              handleTaggedGeneric(
                cond,
                thenStep,
                elseStep,
                cfg,
                tag,
                config,
              )
          }
        case _ => transform(taggedInnerStep, config)
      }
    case BlockStep(StepBlock(stmts)) =>
      val Config(newEnv, newSteps) = stmts.foldLeft(config.clear) {
        case (config, stmt) => transform(stmt.step, config)
      }
      if (newSteps.isEmpty) config(newEnv)
      else config(newEnv) :+ newSteps.toBlockStep

    case RepeatStep(c, b) =>
      val newBody = transform(b, config.clear).steps.toBlockStep
      config :+ RepeatStep(c, newBody)

    case s @ ForEachStep(_, _, _, _, body) =>
      val newBody = transform(body, config.clear).steps.toBlockStep
      config :+ s.copy(body = newBody)

    case s @ ForEachIntegerStep(_, _, _, _, _, _, body) =>
      val newBody = transform(body, config.clear).steps.toBlockStep
      config :+ s.copy(body = newBody)

    case s @ ForEachOwnPropertyKeyStep(_, _, _, _, _, body) =>
      val newBody = transform(body, config.clear).steps.toBlockStep
      config :+ s.copy(body = newBody)

    case s @ ForEachParseNodeStep(_, _, body) =>
      val newBody = transform(body, config.clear).steps.toBlockStep
      config :+ s.copy(body = newBody)

    case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
      val newTry = transform(tryBlock, config.clear).steps.toBlockStep
      val newCatch =
        catchBlock.map(b => transform(b, config.clear).steps.toBlockStep)
      config :+ WrappedTryCatchStep(newTry, catchVar, newCatch)
    case _ => config :+ step
  }

  private class ParameterInlineWalker(
    paramName: String,
    replaceWith: Expression,
  ) extends LangWalker {
    override def walk(expr: Expression): Expression = expr match {
      case ReferenceExpression(ref) =>
        ref match {
          case Variable(name, None, _, _) =>
            if (name == paramName) replaceWith else expr
          case x => ReferenceExpression(walk(x))
        }
      case _ => super.walk(expr)
    }

    override def walk(ref: Reference): Reference = ref match {
      case Variable(name, _, _, _) =>
        if (name == paramName) {
          replaceWith.asInstanceOf[ReferenceExpression].ref
        } else ref
      case x => super.walk(x)
    }
  }

  private def handleTaggedCompletion(
    cond: Condition,
    thenStep: Step,
    elseStep: Option[Step],
    cfg: IfStep.ElseConfig,
    tag: Map[String, String],
    targetVar: String,
    checkType: CompletionType,
    config: Config,
  ): Config = {
    val env = config.env
    val thenType =
      if (checkType == MayAbrupt) MayAbrupt
      else MayNormal
    val elseType =
      if (checkType == MayAbrupt) MayNormal
      else MayAbrupt

    val thenEnv = env + (targetVar -> thenType)
    val elseEnv = env + (targetVar -> elseType)

    val Config(thenOptEnv, thenSteps) = transform(thenStep, Config(thenEnv))
    val newThen = thenSteps.toBlockStep
    val (newElse, elseOptEnv) = elseStep match {
      case Some(e) =>
        val Config(eEnv, steps) = transform(e, Config(elseEnv))
        (Some(steps.toBlockStep), eEnv)
      case None => (None, elseEnv)
    }
    val mergedEnv = (isTerminal(thenStep), elseStep.map(isTerminal)) match {
      case (true, Some(false)) => elseOptEnv
      case (false, Some(true)) => thenOptEnv
      case _                   => thenOptEnv ++ elseOptEnv
    }

    val flagVar = tag.getOrElse("USE_FLAG", s"${targetVar}_flag")
    rebaseCondition(
      cond,
      Map(
        targetVar -> BinaryCondition(
          ReferenceExpression(Variable(flagVar, None)),
          Eq,
          if (checkType == MayAbrupt) EnumLiteral("abrupt")
          else EnumLiteral("normal"),
        ),
      ),
    ) match {
      case Some(newCond) =>
        config(mergedEnv) :+ TaggedStep(
          IfStep(newCond, newThen, newElse, cfg),
          tag,
        )
      // TODO Can we ignore ElseStep? If not, how can we handle it?
      case None =>
        config(mergedEnv) :+ newThen
    }
  }

  private def handleTaggedGeneric(
    cond: Condition,
    thenStep: Step,
    elseStep: Option[Step],
    cfg: IfStep.ElseConfig,
    tag: Map[String, String],
    config: Config,
  ): Config = {
    val env = config.env
    val Config(thenOptEnv, thenSteps) = transform(thenStep, Config(env))
    val newThen = thenSteps.toBlockStep
    val (newElse, elseOptEnv) = elseStep match {
      case Some(e) =>
        val Config(eEnv, steps) = transform(e, Config(env))
        (Some(steps.toBlockStep), eEnv)
      case None => (None, env)
    }
    val mergedEnv = (isTerminal(thenStep), elseStep.map(isTerminal)) match {
      case (true, Some(false) | None) => elseOptEnv
      case (false, Some(true))        => thenOptEnv
      case _                          => thenOptEnv ++ elseOptEnv
    }
    rebaseCondition(cond, Map()) match {
      case Some(newCond) =>
        config(mergedEnv) :+ TaggedStep(
          IfStep(newCond, newThen, newElse, cfg),
          tag,
        )
      // TODO Can we ignore ElseStep? If not, how can we handle it?
      case None =>
        config(mergedEnv) :+ newThen
    }
  }

  def isTerminal(stmt: Step): Boolean = stmt match {
    case ReturnStep(_) => true
    case ThrowStep(_)  => true
    case BlockStep(StepBlock(steps)) =>
      steps.lastOption.exists(it => isTerminal(it.step))
    case IfStep(_, t, Some(e), _)           => isTerminal(t) && isTerminal(e)
    case WrappedTryCatchStep(t, _, Some(c)) => isTerminal(t) && isTerminal(c)
    case _                                  => false
  }

  def annotateStep(
    step: Step,
    name: String,
    value: String,
  ): TaggedStep = step match {
    case TaggedStep(s, tag) => TaggedStep(s, tag + (name -> value))
    case x                  => TaggedStep(x, Map(name -> value))
  }

  def getHoistedFlagSetting(
    flagName: String,
    flag: String, // "abrupt" or "normal"
    env: TypeEnv,
  ): Step = LetStep(Variable(flagName, None), EnumLiteral(flag))

  def rebaseCondition(
    cond: Condition,
    completionCondition: Map[String, Condition],
  ): Option[Condition] = cond match {
    case PredicateCondition(
          ReferenceExpression(Variable(targetVar, _, _, _)),
          _,
          op,
        ) =>
      op match {
        case Abrupt | Throw | Normal | Return =>
          completionCondition.get(targetVar)
        case _ => Some(cond)
      }
    case compoundCond @ CompoundCondition(left, _, right) =>
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

  def wrap(
    config: Config,
    x: String,
    expr: Expression,
    ctype: CompletionType,
    isDecl: Boolean,
  ): Config = {
    val flagName = s"${x}_flag"
    val catchVar = s"_${x}_err"
    val step =
      if (isDecl) LetStep(Variable(x), expr)
      else SetStep(Variable(x), expr)
    def aux(flag: String): Step = LetStep(Variable(flagName), EnumLiteral(flag))
    ctype match {
      case MayCompletion =>
        val catchStmts = List(
          SetStep(
            Variable(x),
            ReferenceExpression(Variable(catchVar, None)),
          ),
          getHoistedFlagSetting(flagName, "abrupt", config.env),
        )
        config :+ WrappedTryCatchStep(
          List(step, aux("normal")).toBlockStep,
          Variable(catchVar),
          Some(catchStmts.toBlockStep),
        )
      case MayNormal     => config :+ step :+ aux("normal")
      case MayAbrupt     => config :+ step :+ aux("abrupt")
      case NotCompletion => config :+ step
    }
  }

  def transform(
    expr: Expression,
    config: Config,
  ): (Expression, CompletionType) = expr match {
    case InvokeAbstractOperationExpression("Completion", args, _) =>
      (args.head, MayCompletion)
    case InvokeAbstractOperationExpression("NormalCompletion", args, _) =>
      (args.head, MayNormal)
    case InvokeAbstractOperationExpression("ThrowCompletion", args, _) =>
      (args.head, MayAbrupt)
    case InvokeAbstractOperationExpression("AbruptCompletion", args, _) =>
      (args.head, MayAbrupt)
    case AbstractClosureExpression(params, captured, body) =>
      val optimizedBody = transform(body, config.clear).steps.toBlockStep
      (
        AbstractClosureExpression(params, captured, optimizedBody),
        NotCompletion,
      )
    case ReferenceExpression(Variable(name, _, _, _)) => (expr, config(name))
    case ReturnIfAbruptExpression(expr, _)            => (expr, NotCompletion)
    case _                                            => (expr, NotCompletion)
  }
}

private object CompletionCheckPattern {
  def unapply(step: Step): Option[(CompletionType, String)] = step match {
    case IfStep(cond, thenStep, elseStep, config) => traverseCondition(cond)
    case _                                        => None
  }

  private def traverseCondition(
    cond: Condition,
  ): Option[(CompletionType, String)] =
    cond match {
      case PredicateCondition(expr, _, op) =>
        op match {
          case Abrupt | Throw => Some((MayAbrupt, extractVarName(expr)))
          case Normal         => Some((MayNormal, extractVarName(expr)))
          case _              => None
        }
      case CompoundCondition(left, op, right) =>
        traverseCondition(left).orElse(traverseCondition(right))
      case _ => None
    }

  private def extractVarName(expr: Expression) = expr match {
    case ReferenceExpression(Variable(x, _, _, _)) => x
    case err =>
      raise(
        s"Expected Reference Expression for extractVarName, but got '${err.toString}'",
      )
  }
}

private class ValueAccessUnwrapper(env: TypeEnv) extends LangWalker {

  override def walk(step: Step): Step = step match {
    case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
      WrappedTryCatchStep(
        walk(tryBlock),
        walk(catchVar),
        walkOpt(catchBlock, walk),
      )
    case TaggedStep(innerStep, tag) =>
      TaggedStep(walk(innerStep), tag)
    case _ => super.walk(step)
  }

  override def walk(expr: Expression): Expression = expr match {
    // Unwrap .[[Value]] access on known completion types
    case ReferenceExpression(
          Access(Variable(varName, _, _, _), "Value", _, _),
        ) =>
      env(varName) match {
        case NotCompletion => super.walk(expr)
        case _ =>
          ReferenceExpression(Variable(varName, Some("value_unwrapped")))
      }
    // Unwrap Completion AO calls
    case completionAO @ InvokeAbstractOperationExpression(name, args, _)
        if name.contains("Completion") =>
      if (args.length > 1)
        raise(
          s"Completion AO Call should contain up to one argument:\n\t$completionAO",
        )
      args.head
    // AO calls with completion argument unpacking
    case aoExpr @ InvokeAbstractOperationExpression(name, args, _) =>
      val newArgs = args.flatMap {
        case x @ ReferenceExpression(v @ Variable(targetVar, nt, _, _))
            if nt.isEmpty =>
          env(targetVar) match {
            case MayAbrupt | MayNormal | MayCompletion =>
              // Plug in x_flag directly — no numeric conversion
              List(
                ReferenceExpression(Variable(s"${targetVar}_flag", None)),
                x.copy(v.copy(nt = Some("comp_split"))),
              )
            case _ => Some(x)
          }
        case c @ InvokeAbstractOperationExpression(
              innerCallName,
              innerArgs,
              _,
            ) if innerCallName.contains("Completion") =>
          if (innerArgs.length > 1)
            raise(
              s"Completion AO Call should contain up to one argument:\n\t$c",
            )
          innerCallName match {
            case "NormalCompletion" =>
              List(EnumLiteral("normal"), innerArgs.head)
            case "ThrowCompletion" | "AbruptCompletion" =>
              List(EnumLiteral("abrupt"), innerArgs.head)
            case "Completion" =>
              raise(
                s"Cannot unpack the raw completion object: $c",
              )
            case _ => Some(c.copy(args = innerArgs.map(walk)))
          }
        case x => Some(walk(x))
      }
      aoExpr.copy(args = newArgs)
    case _ => super.walk(expr)
  }
}

extension (iter: Iterable[Step]) {
  def toBlockStep: Step = iter.toList match {
    case (b: BlockStep) :: Nil => b
    case list =>
      BlockStep(StepBlock(list.flatMap {
        case BlockStep(StepBlock(steps)) => steps
        case x                           => List(SubStep(None, x))
      }))
  }
}
