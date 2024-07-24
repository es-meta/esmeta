package esmeta.compiler

import esmeta.MANUALS_DIR
import esmeta.ir.{
  Type => IRType,
  UnknownType => IRUnknownType,
  Param => IRParam,
  *,
}
import esmeta.ir.util.{Walker => IRWalker}
import esmeta.lang.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker}
import esmeta.spec.*
import esmeta.ty.*
import esmeta.util.ManualInfo
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.math.MathContext.UNLIMITED
import scala.collection.mutable.ListBuffer
import esmeta.ir.util.isPure

/** compiler from metalangauge to IR */
object Compiler:
  def apply(
    spec: Spec,
    log: Boolean = false,
  ): Program = new Compiler(spec, log).result

/** extensible helper of compiler from metalangauge to IR */
class Compiler(
  spec: Spec,
  log: Boolean = false,
) {

  /** compiled specification */
  lazy val result: Program =
    for (algo <- spec.algorithms) compile(algo)
    Program(funcs.toList, spec)

  /** load manually created AOs */
  val manualAlgos = (for {
    file <- ManualInfo.funcFiles
    func = Func.fromFile(file.toString)
  } yield func).toList
  val manualAlgoNames = manualAlgos.map(_.name).toSet

  /** compiled algorithms */
  val funcs: ListBuffer[Func] = ListBuffer.from(manualAlgos)

  /** load manual compile rules */
  val manualRules: ManualInfo.CompileRule = ManualInfo.compileRule

  /** load manual compile rules for expressions */
  val exprRules: Map[String, Expr] = for {
    (yet, str) <- manualRules.getOrElse("expr", Map())
  } yield yet -> Expr.from(str)

  /** load manual compile rules for instructions */
  val instRules: Map[String, Inst] = for {
    (yet, str) <- manualRules.getOrElse("inst", Map())
  } yield yet -> Inst.from(str)

  /** get unused manual compile rules */
  var unusedRules: Set[String] = exprRules.keySet ++ instRules.keySet

  /** grammar */
  def grammar: Grammar = spec.grammar

  /** list of function names which need to replace return step return to resumed
    * step since they have no note step for that return
    */
  val fixReturnAOs =
    List("GeneratorStart", "AsyncBlockStart", "AsyncGeneratorStart")

  /** list of function names which need to replace head to built-in when
    * creating closure (ex: Await)
    */
  val fixClosurePrefixAOs: List[scala.util.matching.Regex] =
    List(
      "Await".r,
      "MakeArg.*".r, // MakeArgGetter, MakeArgSetter
      "ClassTail\\[\\d,\\d\\]\\.ClassDefinitionEvaluation".r,
      "ExecuteAsyncModule".r,
      "FinishDynamicImport".r,
      "INTRINSICS\\.Object\\.fromEntries".r,
      "AsyncFromSyncIteratorContinuation".r,
      // CreateResolvingFunctions
      "NewPromiseCapability".r,
      // PerformPromiseAll
      // PerformPromiseAllSettled
      // PerformPromiseAny
      "INTRINSICS.Promise.prototype.finally".r,
      "AsyncGeneratorAwaitReturn".r,
      "INTRINSICS.Proxy.revocable".r,
    )

  /* set of shorthands
   *
   * NOTE: https://github.com/tc39/ecma262/issues/2384
   */
  val shorthands = Set(
    "IfAbruptCloseIterator",
    "IfAbruptRejectPromise",
  )

  /* set of function names not to compile */
  // TODO why "INTRINSICS.Array.prototype[@@unscopables]" is excluded?
  val excluded = manualAlgoNames ++ shorthands ++ Set(
    "INTRINSICS.Array.prototype[@@unscopables]",
  )

  /* get function kind */
  def getKind(head: Head): FuncKind = {
    import FuncKind.*
    head match {
      case head: AbstractOperationHead       => AbsOp
      case head: NumericMethodHead           => NumMeth
      case head: SyntaxDirectedOperationHead => SynDirOp
      case head: ConcreteMethodHead          => ConcMeth
      case head: InternalMethodHead          => InternalMeth
      case head: BuiltinHead                 => Builtin
    }
  }

  /** get prefix instructions for builtin functions */
  def getBuiltinPrefix(ps: List[Param]): List[Inst] =
    import ParamKind.*
    if (ps.exists(_.kind == Ellipsis)) Nil
    else {
      // add bindings for original arguments
      val argsLen = toStrERef(NAME_ARGS_LIST, "length")
      var remaining = ps.count(_.kind == Normal)
      ps.flatMap {
        case Param(name, _, Variadic) if remaining == 0 =>
          List(ILet(Name(name), ENAME_ARGS_LIST))
        case Param(name, _, Variadic) =>
          List(
            ILet(Name(name), EList(Nil)),
            IWhile(
              lessThan(EMath(BigDecimal(remaining, UNLIMITED)), argsLen),
              IPush(EPop(ENAME_ARGS_LIST, true), toERef(Name(name)), false),
            ),
          )
        case Param(name, _, kind) =>
          if (kind == Normal) remaining -= 1
          List(
            IIf(
              lessThan(zero, argsLen),
              ILet(Name(name), EPop(ENAME_ARGS_LIST, true)),
              ILet(Name(name), EAbsent()),
            ),
          )
      }
    }

  /** compile with a new scope and convert it into an instruction */
  def compileWithScope(fb: FuncBuilder, step: Step): Inst =
    fb.newScope(compile(fb, step))

  /** get body instruction */
  def addFunc(
    fb: FuncBuilder,
    body: Step,
    prefix: List[Inst] = Nil,
  ): Unit =
    if (!excluded.contains(fb.name))
      val inst = compileWithScope(fb, body)
      funcs += fb.getFunc(prefix match
        case Nil => inst
        case _   => ISeq(prefix ++ inst.toList),
      )

  /** compile an algorithm to an IR function */
  // TODO consider refactor
  def compile(algo: Algorithm): Unit =
    val prefix = algo.head match
      case head: BuiltinHead => getBuiltinPrefix(head.params)
      case _                 => Nil
    addFunc(
      fb = FuncBuilder(
        spec,
        getKind(algo.head),
        algo.head.fname,
        algo.head.funcParams.map(compile),
        compile(algo.retTy),
        algo,
      ),
      body = algo.body,
      prefix = prefix,
    )

  /** compile algorithm steps */
  def compile(
    fb: FuncBuilder,
    step: Step,
  ): Unit = fb.withLang(step)(step match {
    case LetStep(x, expr) =>
      fb.addInst(ILet(compile(x), compile(fb, expr)))
    case SetStep(ref, expr) =>
      fb.addInst(IAssign(compile(fb, ref), compile(fb, expr)))
    case SetFieldsWithIntrinsicsStep(ref) =>
      fb.addInst(IAssign(compile(fb, ref), EGLOBAL_INTRINSICS))
    case IfStep(cond, thenStep, elseStep) =>
      import CompoundConditionOperator.*
      // apply shortcircuit for invoke expression
      val condExpr = cond match
        case CompoundCondition(_, And | Or, right) if hasInvokeExpr(right) =>
          val (x, xExpr) = fb.newTIdWithExpr
          compileShortCircuit(fb, x, cond)
          xExpr
        case _ => compile(fb, cond)
      fb.addInst(
        IIf(
          condExpr,
          compileWithScope(fb, thenStep),
          elseStep.fold(emptyInst)(compileWithScope(fb, _)),
        ),
      )
    case ReturnStep(expr) =>
      val e = expr.fold(EUndef())(compile(fb, _))
      fb.returnContext match
        case None          => fb.addInst(IReturn(e))
        case Some(context) => fb.addReturnToResume(context, e)
    case AssertStep(cond) =>
      fb.addInst(IAssert(compile(fb, cond)))
    case ForEachStep(ty, x, expr, true, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, compile(fb, expr)),
        IAssign(i, zero),
      )
      fb.addInst(
        IWhile(
          lessThan(iExpr, toStrERef(list, "length")),
          fb.newScope {
            fb.addInst(ILet(compile(x), toERef(list, iExpr)))
            compile(fb, body)
            fb.addInst(IAssign(i, inc(iExpr)))
          },
        ),
      )
    case ForEachStep(ty, x, expr, false, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, compile(fb, expr)),
        IAssign(i, toStrERef(list, "length")),
      )
      fb.addInst(
        IWhile(
          lessThan(zero, iExpr),
          fb.newScope {
            fb.addInst(IAssign(i, sub(iExpr, one)))
            fb.addInst(ILet(compile(x), toERef(list, iExpr)))
            compile(fb, body)
          },
        ),
      )
    case ForEachIntegerStep(x, low, high, ascending, body) =>
      val (start, end) = if (ascending) (low, high) else (high, low)
      val (i, iExpr) = compileWithExpr(x)
      fb.addInst(ILet(i, compile(fb, start)))
      fb.addInst(
        IWhile(
          if (ascending) not(lessThan(compile(fb, end), iExpr))
          else not(lessThan(iExpr, compile(fb, end))),
          fb.newScope {
            compile(fb, body)
            val op = if (ascending) add(_, _) else sub(_, _)
            fb.addInst(IAssign(i, op(iExpr, one)))
          },
        ),
      )
    case ForEachOwnPropertyKeyStep(x, obj, cond, ascending, order, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      val (key, keyExpr) = compileWithExpr(x)
      val intSorted = order == ForEachOwnPropertyKeyStepOrder.NumericIndexOrder
      fb.addInst(
        IAssign(list, EKeys(toStrERef(compile(fb, obj), "SubMap"), intSorted)),
        if (ascending) IAssign(i, zero)
        else IAssign(i, toStrERef(list, "length")),
        IWhile(
          if (ascending) lessThan(iExpr, toStrERef(list, "length"))
          else lessThan(zero, iExpr),
          fb.newScope {
            if (!ascending) fb.addInst(IAssign(i, sub(iExpr, one)))
            fb.addInst(ILet(key, toERef(list, iExpr)))
            fb.addInst(
              IIf(
                compile(fb, cond),
                compileWithScope(fb, body),
                emptyInst,
              ),
            )
            if (ascending) fb.addInst(IAssign(i, inc(iExpr)))
          },
        ),
      )
    case ForEachParseNodeStep(x, expr, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      val (length, lengthExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, EGetChildren(compile(fb, expr))),
        IAssign(i, zero),
        IAssign(length, toStrERef(list, "length")),
      )
      fb.addInst(
        IWhile(
          lessThan(iExpr, lengthExpr),
          fb.newScope {
            fb.addInst(ILet(compile(x), toERef(list, iExpr)))
            compile(fb, body)
            fb.addInst(IAssign(i, inc(iExpr)))
          },
        ),
      )
    case ThrowStep(expr) =>
      val comp = EComp(EENUM_THROW, compile(fb, expr), EENUM_EMPTY)
      fb.addInst(IReturn(comp))
    case PerformStep(expr) =>
      compile(fb, expr) match
        case era: EReturnIfAbrupt => fb.addInst(IExpr(era))
        case _                    =>
    case PerformBlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(fb, substep.step)
    case AppendStep(expr, ref) =>
      fb.addInst(IPush(compile(fb, expr), ERef(compile(fb, ref)), false))
    case PrependStep(expr, ref) =>
      fb.addInst(IPush(compile(fb, expr), ERef(compile(fb, ref)), true))
    case RepeatStep(cond, body) =>
      fb.addInst(
        IWhile(
          cond.fold(EBool(true))(compile(fb, _)),
          compileWithScope(fb, body),
        ),
      )
    case PushCtxtStep(ref) =>
      fb.addInst(IPush(ERef(compile(fb, ref)), EGLOBAL_EXECUTION_STACK, true))
    case NoteStep(note) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context, false) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context, true) =>
      fb.addInst(IExpr(EPop(EGLOBAL_EXECUTION_STACK, true)))
    case RemoveStep(elem, list) =>
      fb.addInst(IRemove(compile(fb, elem), compile(fb, list)))
    case RemoveFirstStep(expr) =>
      fb.addInst(IExpr(EPop(compile(fb, expr), true)))
    case RemoveContextStep(_, _) =>
      fb.addInst(IExpr(EPop(EGLOBAL_EXECUTION_STACK, true)))
    case SetEvaluationStateStep(context, paramOpt, body) =>
      val ctxt = compile(fb, context)
      val contName = fb.nextContName
      addFunc(
        fb = FuncBuilder(
          spec,
          FuncKind.Cont,
          contName,
          toParams(paramOpt),
          fb.retTy,
          fb.algo,
          if (fixReturnAOs contains fb.name) Some(ctxt) else None,
        ),
        body = body,
      )
      fb.addInst(IAssign(toStrRef(ctxt, "ResumeCont"), ECont(contName)))
    case ResumeEvaluationStep(context, argOpt, paramOpt, steps) =>
      val ctxt = compile(fb, context)
      val returnCont = toStrRef(ctxt, "ReturnCont")
      val (eResumeCont, eReturnCont) =
        (toStrERef(ctxt, "ResumeCont"), ERef(returnCont))
      val contName = fb.nextContName
      val ps = toParams(paramOpt)
      val bodyStep = BlockStep(StepBlock(steps))
      addFunc(
        fb = FuncBuilder(
          spec,
          FuncKind.Cont,
          contName,
          ps,
          fb.retTy,
          fb.algo,
        ),
        body = bodyStep,
      )
      fb.addInst(
        IIf(
          isAbsent(eReturnCont),
          IAssign(returnCont, emptyList),
          emptyInst,
        ),
        IPush(ECont(contName), eReturnCont, true),
        ICall(fb.newTId, eResumeCont, argOpt.map(compile(fb, _)).toList),
      )
    case ResumeYieldStep(_, arg, context, param, steps) =>
      val ctxt = compile(fb, context)
      val contName = fb.nextContName
      addFunc(
        fb = FuncBuilder(
          spec,
          FuncKind.Cont,
          contName,
          List(toParam(param)),
          fb.retTy,
          fb.algo,
          if (fixReturnAOs contains fb.name) Some(ctxt) else None,
        ),
        body = BlockStep(StepBlock(steps)),
      )
      fb.addInst(IAssign(toStrRef(ctxt, "ResumeCont"), ECont(contName)))
      fb.addReturnToResume(compile(fb, context), compile(fb, arg))
    case ReturnToResumeStep(context, retStep) =>
      val arg = retStep.expr.fold(EUndef())(compile(fb, _))
      fb.addReturnToResume(compile(fb, context), arg)
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(fb, substep.step)
    case YetStep(yet) =>
      val yetStr = yet.toString(true, false)
      val inst = instRules.get(yetStr).getOrElse(IExpr(EYet(yetStr)))
      unusedRules -= yetStr
      fb.addInst(inst)
  })

  /** compile local variable */
  def compile(x: Variable): Name = Name(x.name)
  def compileWithExpr(x: Variable): (Name, Expr) =
    val n = Name(x.name); (n, ERef(n))

  /** compile references */
  def compile(fb: FuncBuilder, ref: Reference): Ref =
    fb.withLang(ref)(ref match {
      case x: Variable               => compile(x)
      case RunningExecutionContext() => GLOBAL_CONTEXT
      case SecondExecutionContext()  => toRef(GLOBAL_EXECUTION_STACK, EMath(1))
      case CurrentRealmRecord()      => currentRealm
      case ActiveFunctionObject()    => toStrRef(GLOBAL_CONTEXT, "Function")
      case ref: PropertyReference    => compile(fb, ref)
      case AgentRecord()             => GLOBAL_AGENT_RECORD
    })

  def compile(fb: FuncBuilder, ref: PropertyReference): Field =
    val PropertyReference(base, prop) = ref
    val baseRef = compile(fb, base)
    prop match
      case FieldProperty(name)     => Field(baseRef, EStr(name))
      case ComponentProperty(name) => Field(baseRef, EStr(name))
      case BindingProperty(expr) =>
        Field(toStrRef(baseRef, "SubMap"), compile(fb, expr))
      case IndexProperty(index)      => Field(baseRef, compile(fb, index))
      case IntrinsicProperty(intr)   => toIntrinsic(baseRef, intr)
      case NonterminalProperty(name) => Field(baseRef, EStr(name))

  /** compile expressions */
  def compile(fb: FuncBuilder, expr: Expression): Expr =
    fb.withLang(expr)(expr match {
      case StringConcatExpression(exprs) =>
        EVariadic(VOp.Concat, exprs.map(compile(fb, _)))
      case ListConcatExpression(exprs) =>
        EListConcat(exprs.map(compile(fb, _)))
      case ListCopyExpression(expr) => ECopy(compile(fb, expr))
      case RecordExpression("Completion Record", fields) =>
        val fmap = fields.toMap
        val fs @ List(ty, v, tgt) =
          List("Type", "Value", "Target").map(FieldLiteral(_))
        val keys = fmap.keySet
        if (keys != fs.toSet)
          error(s"invalid completion keys: ${keys.mkString(", ")}")
        EComp(
          compile(fb, fmap(ty)),
          compile(fb, fmap(v)),
          compile(fb, fmap(tgt)),
        )
      case RecordExpression(tname, fields) =>
        val props = fields.map {
          case (f, e) => compile(fb, f) -> compile(fb, e)
        }
        val normalized = (Type.normalizeName(tname))
        if normalized == "SubMap" then EMap(normalized, props)
        else ERecord(normalized, props)
      case LengthExpression(ReferenceExpression(ref)) =>
        toStrERef(compile(fb, ref), "length")
      case LengthExpression(expr) =>
        val (x, xExpr) = fb.newTIdWithExpr
        fb.addInst(IAssign(x, compile(fb, expr)))
        toStrERef(x, "length")
      case SubstringExpression(expr, from, to) =>
        ESubstring(
          compile(fb, expr),
          compile(fb, from),
          to.map(compile(fb, _)),
        )
      case TrimExpression(expr, leading, trailing) =>
        (leading, trailing) match {
          case (false, false) => compile(fb, expr)
          case (true, false)  => ETrim(compile(fb, expr), true)
          case (false, true)  => ETrim(compile(fb, expr), false)
          case (true, true)   => ETrim(ETrim(compile(fb, expr), true), false)
        }
      case NumberOfExpression(ReferenceExpression(ref)) =>
        toStrERef(compile(fb, ref), "length")
      case NumberOfExpression(expr) =>
        val (x, xExpr) = fb.newTIdWithExpr
        fb.addInst(IAssign(x, compile(fb, expr)))
        toStrERef(x, "length")
      case IntrinsicExpression(intr) =>
        toEIntrinsic(currentIntrinsics, intr)
      case SourceTextExpression(expr) =>
        ESourceText(compile(fb, expr))
      case CoveredByExpression(code, rule) =>
        EParse(compile(fb, code), compile(fb, rule))
      case GetItemsExpression(nt, expr) =>
        EGetItems(compile(fb, nt), compile(fb, expr))
      case InvokeAbstractOperationExpression(name, args) =>
        val as = args.map(compile(fb, _))
        if simpleOps contains name then simpleOps(name)(as)
        else if shorthands contains name then compileShorthand(fb, name, as)
        else
          val (x, xExpr) = fb.newTIdWithExpr
          val f = EClo(name, Nil)
          fb.addInst(ICall(x, f, as))
          xExpr
      case InvokeNumericMethodExpression(ty, name, args) =>
        val (x, xExpr) = fb.newTIdWithExpr
        val f = EClo(s"$ty::$name", Nil)
        fb.addInst(ICall(x, f, args.map(compile(fb, _))))
        xExpr
      case InvokeAbstractClosureExpression(ref, args) =>
        val (x, xExpr) = fb.newTIdWithExpr
        fb.addInst(ICall(x, ERef(compile(fb, ref)), args.map(compile(fb, _))))
        xExpr
      case InvokeMethodExpression(ref, args) =>
        val Field(base, method) = compile(fb, ref)
        val (b, bExpr) =
          if (isPure(base)) (base, ERef(base))
          else fb.newTIdWithExpr(ERef(base))
        val fexpr = ERef(Field(b, method))
        val (x, xExpr) = fb.newTIdWithExpr
        fb.addInst(ICall(x, fexpr, bExpr :: args.map(compile(fb, _))))
        xExpr
      case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
        // XXX BUG in Static Semancis: CharacterValue
        val baseExpr = compile(fb, base)
        val (x, xExpr) = fb.newTIdWithExpr
        fb.addInst(ISdoCall(x, baseExpr, name, args.map(compile(fb, _))))
        xExpr
      case ReturnIfAbruptExpression(expr, check) =>
        EReturnIfAbrupt(compile(fb, expr), check)
      case ListExpression(entries) =>
        EList(entries.map(compile(fb, _)))
      case IntListExpression(from, fInc, to, tInc, asc) =>
        val (f, fExpr) = fb.newTIdWithExpr
        val (t, tExpr) = fb.newTIdWithExpr
        val (i, iExpr) = fb.newTIdWithExpr
        val (list, listExpr) = fb.newTIdWithExpr
        fb.addInst(
          IAssign(f, compile(fb, from)),
          IAssign(t, compile(fb, to)),
          IAssign(
            i,
            if (asc) (if (fInc) fExpr else inc(fExpr))
            else if (tInc) tExpr
            else dec(tExpr),
          ),
          IAssign(list, EList(Nil)),
        )
        fb.addInst(
          IWhile(
            if (asc) lessThan(iExpr, if (tInc) inc(tExpr) else tExpr)
            else lessThan(if (fInc) dec(fExpr) else fExpr, iExpr),
            fb.newScope {
              fb.addInst(IPush(iExpr, listExpr, false))
              fb.addInst(IAssign(i, if (asc) inc(iExpr) else dec(iExpr)))
            },
          ),
        )
        listExpr
      case yet: YetExpression =>
        val yetStr = yet.toString(true, false)
        unusedRules -= yetStr
        exprRules.get(yetStr).getOrElse(EYet(yetStr))
      case ReferenceExpression(ref) =>
        ERef(compile(fb, ref))
      case MathFuncExpression(op, args) =>
        import MathFuncExpressionOperator.*
        (op, args) match
          case (Max, _)         => EVariadic(VOp.Max, args.map(compile(fb, _)))
          case (Min, _)         => EVariadic(VOp.Min, args.map(compile(fb, _)))
          case (Abs, List(arg)) => EUnary(UOp.Abs, compile(fb, arg))
          case (Floor, List(arg)) => EUnary(UOp.Floor, compile(fb, arg))
          case (Truncate, List(arg)) =>
            val (x, xExpr) = fb.newTIdWithExpr
            fb.addInst(
              IAssign(x, compile(fb, arg)),
              IIf(
                lessThan(ERef(x), zero),
                IAssign(x, neg(floor(neg(xExpr)))),
                IAssign(x, floor(xExpr)),
              ),
            )
            xExpr
          case _ => error(s"invalid math operation: $expr")
      case ConversionExpression(op, expr) =>
        import ConversionExpressionOperator.*
        op match
          case ToApproxNumber => EConvert(COp.ToApproxNumber, compile(fb, expr))
          case ToNumber       => EConvert(COp.ToNumber, compile(fb, expr))
          case ToBigInt       => EConvert(COp.ToBigInt, compile(fb, expr))
          case ToMath         => EConvert(COp.ToMath, compile(fb, expr))
      case ExponentiationExpression(base, power) =>
        EBinary(BOp.Pow, compile(fb, base), compile(fb, power))
      case BinaryExpression(left, op, right) =>
        EBinary(compile(op), compile(fb, left), compile(fb, right))
      case UnaryExpression(op, expr) =>
        EUnary(compile(op), compile(fb, expr))
      case ClampExpression(target, lower, upper) =>
        EClamp(
          compile(fb, target),
          compile(fb, lower),
          compile(fb, upper),
        )
      case expr: MathOpExpression => compile(fb, expr)
      case BitwiseExpression(left, op, right) =>
        EBinary(compile(op), compile(fb, left), compile(fb, right))
      case AbstractClosureExpression(params, captured, body) =>
        val algoName = fb.algo.head.fname
        val (ck, cn, ps, prefix) =
          if (fixClosurePrefixAOs.exists(_.matches(algoName)))
            (
              FuncKind.BuiltinClo,
              fb.nextCloName,
              List(PARAM_THIS, PARAM_ARGS_LIST, PARAM_NEW_TARGET),
              getBuiltinPrefix(params.map(x => Param(x.name, UnknownType))),
            )
          else
            (
              FuncKind.Clo,
              fb.nextCloName,
              params.map(x => IRParam(compile(x), IRUnknownType, false)),
              Nil,
            )
        addFunc(
          fb = FuncBuilder(
            spec,
            ck,
            cn,
            ps,
            IRUnknownType,
            fb.algo,
          ),
          body = body,
          prefix = prefix,
        )
        EClo(cn, captured.map(compile))
      case XRefExpression(XRefExpressionOperator.Algo, id) =>
        EClo(spec.getAlgoById(id).head.fname, Nil)
      case XRefExpression(XRefExpressionOperator.ParamLength, id) =>
        EMath(spec.getAlgoById(id).head.originalParams.length)
      case XRefExpression(XRefExpressionOperator.InternalSlots, id) =>
        // TODO properly handle table column
        EList(for {
          row <- spec.tables(id).rows
          slot = row.head if slot.startsWith("[[") && slot.endsWith("]]")
        } yield EStr(slot.substring(2, slot.length - 2)))
      case SoleElementExpression(list) =>
        toERef(fb, compile(fb, list), zero)
      case CodeUnitAtExpression(base, index) =>
        toERef(fb, compile(fb, base), compile(fb, index))
      case lit: Literal => compile(fb, lit)
    })

  /** compile mathematical operators */
  def compile(fb: FuncBuilder, expr: MathOpExpression): Expr =
    import MathOpExpressionOperator.*
    val MathOpExpression(op, args) = expr
    (op, args.map(compile(fb, _))) match
      case (Neg, List(e))      => EUnary(UOp.Neg, e)
      case (Add, List(l, r))   => EBinary(BOp.Add, l, r)
      case (Mul, List(l, r))   => EBinary(BOp.Mul, l, r)
      case (Sub, List(l, r))   => EBinary(BOp.Sub, l, r)
      case (Pow, List(l, r))   => EBinary(BOp.Pow, l, r)
      case (Expm1, List(e))    => EMathOp(MOp.Expm1, List(e))
      case (Log10, List(e))    => EMathOp(MOp.Log10, List(e))
      case (Log2, List(e))     => EMathOp(MOp.Log2, List(e))
      case (Cos, List(e))      => EMathOp(MOp.Cos, List(e))
      case (Cbrt, List(e))     => EMathOp(MOp.Cbrt, List(e))
      case (Exp, List(e))      => EMathOp(MOp.Exp, List(e))
      case (Cosh, List(e))     => EMathOp(MOp.Cosh, List(e))
      case (Sinh, List(e))     => EMathOp(MOp.Sinh, List(e))
      case (Tanh, List(e))     => EMathOp(MOp.Tanh, List(e))
      case (Acos, List(e))     => EMathOp(MOp.Acos, List(e))
      case (Acosh, List(e))    => EMathOp(MOp.Acosh, List(e))
      case (Asinh, List(e))    => EMathOp(MOp.Asinh, List(e))
      case (Atanh, List(e))    => EMathOp(MOp.Atanh, List(e))
      case (Asin, List(e))     => EMathOp(MOp.Asin, List(e))
      case (Atan2, List(x, y)) => EMathOp(MOp.Atan2, List(x, y))
      case (Atan, List(e))     => EMathOp(MOp.Atan, List(e))
      case (Log1p, List(e))    => EMathOp(MOp.Log1p, List(e))
      case (Log, List(e))      => EMathOp(MOp.Log, List(e))
      case (Sin, List(e))      => EMathOp(MOp.Sin, List(e))
      case (Sqrt, List(e))     => EMathOp(MOp.Sqrt, List(e))
      case (Tan, List(e))      => EMathOp(MOp.Tan, List(e))
      case _ => error(s"invalid math operationr: $op with $args")

  /** compile binary operators */
  def compile(op: BinaryExpressionOperator): BOp = op match
    case BinaryExpressionOperator.Add => BOp.Add
    case BinaryExpressionOperator.Sub => BOp.Sub
    case BinaryExpressionOperator.Mul => BOp.Mul
    case BinaryExpressionOperator.Div => BOp.Div
    case BinaryExpressionOperator.Mod => BOp.Mod

  /** compile unary operators */
  def compile(op: UnaryExpressionOperator): UOp = op match
    case UnaryExpressionOperator.Neg => UOp.Neg

  /** compile literals */
  def compile(fb: FuncBuilder, lit: Literal): Expr = lit match {
    case ThisLiteral()      => ENAME_THIS
    case NewTargetLiteral() => ENAME_NEW_TARGET
    case HexLiteral(hex, name) =>
      if (name.isDefined) ECodeUnit(hex.toChar) else EMath(hex)
    case CodeLiteral(code) => EStr(code)
    case NonterminalLiteral(ordinal, name, flags) =>
      val ntNames = fb.ntBindings.map(_._1)
      // TODO ClassTail[0,3].Contains
      if (ntNames contains name) {
        val xs = fb.ntBindings.filter(_._1 == name)
        xs(ordinal.getOrElse(1) - 1) match
          case (_, base, None)      => base
          case (_, base, Some(idx)) => toERef(fb, base, EMath(idx))
      } else ENt(name, flags.map(_ startsWith "+"))
    case EnumLiteral(name)                   => EEnum(name)
    case StringLiteral(s)                    => EStr(s)
    case FieldLiteral(field)                 => EStr(field)
    case SymbolLiteral(sym)                  => toERef(GLOBAL_SYMBOL, EStr(sym))
    case ProductionLiteral(lhsName, rhsName) =>
      // XXX need to handle arguments, children?
      val (lhs, rhsIdx) = getProductionData(lhsName, rhsName)
      ESyntactic(lhsName, lhs.params.map(_ => true), rhsIdx, Nil)
    case ErrorObjectLiteral(name) =>
      val proto = Intrinsic(name, List("prototype"))
      ERecord(
        "OrdinaryObject",
        List(
          EStr("Prototype") -> toEIntrinsic(currentIntrinsics, proto),
          EStr("ErrorData") -> EUndef(),
        ),
      )
    case _: PositiveInfinityMathValueLiteral => EInfinity(pos = true)
    case _: NegativeInfinityMathValueLiteral => EInfinity(pos = false)
    case DecimalMathValueLiteral(n)          => EMath(n)
    case MathConstantLiteral(pre, name) =>
      val expr = name match
        case "π" => EGLOBAL_MATH_PI
        case _   => EYet(s"<mathematical constant: $name>")
      if (pre == 1) expr else EBinary(BOp.Mul, EMath(pre), expr)
    case NumberLiteral(n)       => ENumber(n)
    case BigIntLiteral(n)       => EBigInt(n)
    case TrueLiteral()          => EBool(true)
    case FalseLiteral()         => EBool(false)
    case UndefinedLiteral()     => EUndef()
    case NullLiteral()          => ENull()
    case AbsentLiteral()        => EAbsent()
    case UndefinedTypeLiteral() => EGLOBAL_UNDEF_TYPE
    case NullTypeLiteral()      => EGLOBAL_NULL_TYPE
    case BooleanTypeLiteral()   => EGLOBAL_BOOL_TYPE
    case StringTypeLiteral()    => EGLOBAL_STRING_TYPE
    case SymbolTypeLiteral()    => EGLOBAL_SYMBOL_TYPE
    case NumberTypeLiteral()    => EGLOBAL_NUMBER_TYPE
    case BigIntTypeLiteral()    => EGLOBAL_BIGINT_TYPE
    case ObjectTypeLiteral()    => EGLOBAL_OBJECT_TYPE
  }

  /** compile bitwise operations */
  def compile(op: BitwiseExpressionOperator): BOp = op match {
    case BitwiseExpressionOperator.BAnd => BOp.BAnd
    case BitwiseExpressionOperator.BOr  => BOp.BOr
    case BitwiseExpressionOperator.BXOr => BOp.BXOr
  }

  /** compile branch conditions */
  def compile(fb: FuncBuilder, cond: Condition): Expr =
    fb.withLang(cond)(cond match {
      case ExpressionCondition(expr) =>
        compile(fb, expr)
      case InstanceOfCondition(expr, neg, tys) =>
        val xExpr = compile(fb, expr)
        val e = tys.map(toETypeCheck(xExpr, _)).reduce(or(_, _))
        if (neg) not(e) else e
      case HasFieldCondition(ref, neg, field) =>
        val e = isAbsent(toERef(compile(fb, ref), compile(fb, field)))
        if (neg) e else not(e)
      case HasBindingCondition(ref, neg, binding) =>
        val e = isAbsent(
          toERef(compile(fb, ref), EStr("SubMap"), compile(fb, binding)),
        )
        if (neg) e else not(e)
      // XXX need to be generalized?
      case ProductionCondition(nt, lhsName, rhsName) =>
        val base = compile(fb, nt)
        val (_, rhsIdx) = getProductionData(lhsName, rhsName)
        fb.ntBindings ++= List((rhsName, base, Some(0)))
        ETypeCheck(base, EStr(lhsName + rhsIdx))
      case PredicateCondition(expr, neg, op) =>
        import PredicateConditionOperator.*
        val x = compile(fb, expr)
        val cond = op match {
          case Abrupt =>
            val tv = toERef(fb, x, EStr("Type"))
            and(EIsCompletion(x), not(is(tv, EENUM_NORMAL)))
          case NeverAbrupt =>
            val tv = toERef(fb, x, EStr("Type"))
            or(not(EIsCompletion(x)), is(tv, EENUM_NORMAL))
          case op @ (Normal | Throw | Return | Break | Continue) =>
            val tv = toERef(fb, x, EStr("Type"))
            val expected = op match
              case Normal   => EENUM_NORMAL
              case Throw    => EENUM_THROW
              case Return   => EENUM_RETURN
              case Break    => EENUM_BREAK
              case Continue => EENUM_CONTINUE
            and(EIsCompletion(x), is(tv, expected))
          case Finite =>
            not(
              or(is(x, ENumber(Double.NaN)), or(is(x, posInf), is(x, negInf))),
            )
          case Duplicated =>
            EDuplicated(x)
          case Present =>
            not(isAbsent(x))
          case Empty =>
            val lv = toERef(fb, x, EStr("length"))
            is(lv, zero)
          case StrictMode => T // XXX assume strict mode
          case ArrayIndex => EIsArrayIndex(x)
          case FalseToken => is(ESourceText(x), EStr("false"))
          case TrueToken  => is(ESourceText(x), EStr("true"))
          case DataProperty =>
            val (b, bExpr) = fb.newTIdWithExpr
            fb.addInst(ICall(b, dataPropClo, List(x)))
            bExpr
          case AccessorProperty =>
            val (b, bExpr) = fb.newTIdWithExpr
            fb.addInst(ICall(b, accessorPropClo, List(x)))
            bExpr
          case FullyPopulated =>
            val dataFields =
              List("Value", "Writable", "Enumerable", "Configurable")
            val accessorFields =
              List("Get", "Set", "Enumerable", "Configurable")
            or(hasFields(fb, x, dataFields), hasFields(fb, x, accessorFields))
          case Nonterminal =>
            ETypeCheck(x, EStr("Nonterminal"))
        }
        if (neg) not(cond) else cond
      case IsAreCondition(left, neg, right) =>
        val es = for (lexpr <- left) yield {
          val l = compile(fb, lexpr)
          val e = right
            .map(r => EBinary(BOp.Eq, l, compile(fb, r)))
            .reduce(or(_, _))
          if (neg) not(e) else e
        }
        es.reduce(and(_, _))
      case BinaryCondition(left, op, right) =>
        import BinaryConditionOperator.*
        lazy val l = compile(fb, left)
        lazy val r = compile(fb, right)
        op match {
          case Eq               => EBinary(BOp.Equal, l, r)
          case NEq              => not(EBinary(BOp.Equal, l, r))
          case LessThan         => lessThan(l, r)
          case LessThanEqual    => not(lessThan(r, l))
          case GreaterThan      => lessThan(r, l)
          case GreaterThanEqual => not(lessThan(l, r))
          case SameCodeUnits    => EBinary(BOp.Eq, l, r)
        }
      case InclusiveIntervalCondition(left, neg, from, to) =>
        lazy val l = compile(fb, left)
        lazy val f = compile(fb, from)
        lazy val t = compile(fb, to)
        val cond = not(or(lessThan(l, f), lessThan(t, l)))
        if (neg) not(cond) else cond
      case ContainsCondition(list, neg, target) =>
        import ContainsConditionTarget.*
        lazy val l = compile(fb, list)
        val e = target match
          case Expr(expr) =>
            EContains(l, compile(fb, expr))
          case WhoseField(tyOpt, fieldName, expr) =>
            val x = fb.newTId
            lazy val c = is(toStrERef(x, fieldName), compile(fb, expr))
            compile(fb, l, tyOpt, x, c)
          case SuchThat(tyOpt, x, cond) =>
            lazy val c = compile(fb, cond)
            compile(fb, l, tyOpt, Name(x.name), c)
        if (neg) not(e) else e
      case CompoundCondition(left, op, right) =>
        lazy val l = compile(fb, left)
        lazy val r = compile(fb, right)
        op match
          case CompoundConditionOperator.And   => and(l, r)
          case CompoundConditionOperator.Or    => or(l, r)
          case CompoundConditionOperator.Imply => or(not(l), r)
    })

  /** compile contains condition with additional constraints */
  def compile(
    fb: FuncBuilder,
    list: Expr,
    tyOpt: Option[Type],
    x: Local,
    givenCond: => Expr,
  ): Expr =
    lazy val cond = givenCond
    val (l, lExpr) = fb.newTIdWithExpr
    val (i, iExpr) = fb.newTIdWithExpr
    val (b, bExpr) = fb.newTIdWithExpr
    fb.addInst(
      IAssign(l, list),
      IAssign(i, zero),
      IAssign(b, F),
      IWhile(
        and(not(bExpr), lessThan(iExpr, toStrERef(l, "length"))),
        fb.newScope {
          fb.addInst(
            x match
              case x: Name => ILet(x, toERef(l, iExpr))
              case x: Temp => IAssign(x, toERef(l, iExpr)),
          )
          fb.addInst(
            IAssign(
              b,
              tyOpt.fold(cond)(ty => and(toETypeCheck(ERef(x), ty), cond)),
            ),
            IAssign(i, add(iExpr, one)),
          )
        },
      ),
    )
    bExpr

  /** compile algorithm parameters */
  def compile(param: Param): IRParam = {
    val Param(name, ty, skind) = param
    val optional = skind == ParamKind.Optional
    IRParam(Name(name), compile(ty), optional, Some(param))
  }

  /** compile types */
  def compile(ty: Type): IRType =
    IRType(TyCompiler.walk(ty.ty), Some(ty))

  /** compile shorthands */
  // NOTE: arguments for shorthands are named local identifiers
  def compileShorthand(
    fb: FuncBuilder,
    fname: String,
    args: List[Expr],
  ): Expr =
    val algo = spec.fnameMap(fname)
    val names = args.map {
      case ERef(name: Name) => name
      case e                => error(s"invalid arguments for shorthands: $e")
    }
    val nameMap = (algo.head.funcParams.map(p => Name(p.name)) zip names).toMap
    val renameWalker = new IRWalker {
      override def walk(x: Name) = nameMap.get(x) match
        case Some(x0) => x0
        case None     => x
    }
    val renamed = renameWalker.walk(compileWithScope(fb, algo.body))
    fb.addInst(renamed)
    EUndef() // NOTE: unused expression

  /** handle short circuiting */
  def compileShortCircuit(
    fb: FuncBuilder,
    x: Ref,
    cond: Condition,
  ): Unit = fb.withLang(cond) {
    val xExpr = toERef(x)
    import CompoundConditionOperator.*
    cond match
      case CompoundCondition(left, And, right) =>
        fb.addInst(
          IAssign(x, compile(fb, left)),
          IIf(xExpr, fb.newScope(compileShortCircuit(fb, x, right)), emptyInst),
        )
      case CompoundCondition(left, Or, right) =>
        fb.addInst(
          IAssign(x, compile(fb, left)),
          IIf(xExpr, emptyInst, fb.newScope(compileShortCircuit(fb, x, right))),
        )
      case _ => fb.addInst(IAssign(x, compile(fb, cond)))
  }

  /** check if condition contains invoke expression */
  def hasInvokeExpr(cond: Condition): Boolean = {
    var found = false
    val walker = new LangUnitWalker {
      override def walk(invoke: InvokeExpression): Unit = invoke match
        case InvokeAbstractOperationExpression(name, _)
            if simpleOps contains name =>
        case _ => found = true
    }
    walker.walk(cond)
    found
  }

  /** production helpers */
  def getProductionData(lhsName: String, rhsName: String): (Lhs, Int) =
    val prod = grammar.nameMap(lhsName)
    val rhsList = prod.rhsList.zipWithIndex.filter {
      case (rhs, _) if rhsName == "[empty]" => rhs.isEmpty
      case (rhs, _)                         => rhs.allNames contains rhsName
    }
    rhsList match
      case (rhs, idx) :: Nil => (prod.lhs, idx)
      case _                 => error("invalid production")

  /** instruction helpers */
  inline def toParams(paramOpt: Option[Variable]): List[IRParam] =
    paramOpt.map(toParam(_)).toList
  inline def toParam(x: Variable): IRParam = IRParam(Name(x.name))
  inline def emptyInst = ISeq(List())

  /** expression helpers */
  inline def emptyList = EList(List())
  inline def dataPropClo = EClo("IsDataDescriptor", Nil)
  inline def accessorPropClo = EClo("IsAccessorDescriptor", Nil)

  /** literal helpers */
  def zero = EMath(BigDecimal(0, UNLIMITED))
  def one = EMath(BigDecimal(1, UNLIMITED))
  def posInf = ENumber(Double.PositiveInfinity)
  def negInf = ENumber(Double.NegativeInfinity)
  def T = EBool(true)
  def F = EBool(false)

  /** operation helpers */
  inline def isAbsent(x: Expr) = EBinary(BOp.Eq, x, EAbsent())
  inline def isIntegral(x: Expr) =
    val m = EConvert(COp.ToMath, x)
    and(ETypeCheck(x, EStr("Number")), is(m, floor(m)))
  def not(expr: Expr) = expr match
    case EBool(b)              => EBool(!b)
    case EUnary(UOp.Not, expr) => expr
    case _                     => EUnary(UOp.Not, expr)
  inline def neg(expr: Expr) = EUnary(UOp.Neg, expr)
  inline def floor(expr: Expr) = EUnary(UOp.Floor, expr)
  inline def lessThan(l: Expr, r: Expr) = EBinary(BOp.Lt, l, r)
  inline def inc(e: Expr) = add(e, one)
  inline def dec(e: Expr) = sub(e, one)
  inline def add(l: Expr, r: Expr) = EBinary(BOp.Add, l, r)
  inline def sub(l: Expr, r: Expr) = EBinary(BOp.Sub, l, r)
  inline def and(l: Expr, r: Expr) = EBinary(BOp.And, l, r)
  inline def or(l: Expr, r: Expr) = EBinary(BOp.Or, l, r)
  inline def is(l: Expr, r: Expr) = EBinary(BOp.Eq, l, r)
  inline def hasFields(
    fb: FuncBuilder,
    base: Expr,
    fs: List[String],
  ): Expr =
    val conds = fs.map(f => isAbsent(toERef(fb, base, EStr(f))))
    not(conds.reduce { case (a, b) => or(a, b) })

  /** simple operations */
  type SimpleOp = PartialFunction[List[Expr], Expr]
  def arityCheck(pair: (String, SimpleOp)): (String, SimpleOp) = {
    val (name, f) = pair
    name -> (args =>
      optional(f(args)).getOrElse(
        error(s"invalid arguments: $name(${args.mkString(", ")})"),
      ),
    )
  }
  val simpleOps: Map[String, SimpleOp] = Map(
    arityCheck("ParseText" -> { case List(code, rule) => EParse(code, rule) }),
    arityCheck("Type" -> { case List(expr) => ETypeOf(expr) }),
    arityCheck("ReturnIfAbrupt" -> {
      case List(expr) => EReturnIfAbrupt(expr, true)
    }),
  )
}
