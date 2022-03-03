package esmeta.spec.util

import esmeta.MANUALS_DIR
import esmeta.ir.{Type => IRType, *}
import esmeta.lang.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker}
import esmeta.spec.{Param => SParam, Type => SType, *}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.ListBuffer

/** Compiler from metalangauge to IR */
class Compiler(val spec: Spec) {

  /** compiled specification */
  def result: Program = {
    // compile algorithms in spec
    for (algo <- spec.algorithms) compile(algo)

    // load manually created AOs
    val manualFuncs: ListBuffer[Func] = ListBuffer()
    for (file <- walkTree(MANUALS_DIR) if irFilter(file.getName))
      manualFuncs += Func.fromFile(file.toString)

    // filter manual functions
    val manualNames = manualFuncs.map(_.name)
    val filtered = funcs.filter(f => !manualNames.contains(f.name))

    // result
    val program = Program(filtered.appendAll(manualFuncs).toList)

    // connect backward edge to a given specification
    program.spec = spec

    program
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private val manualRules: Map[String, Inst] = (for {
    (yet, inst) <- readJson[Map[String, String]](s"$MANUALS_DIR/rule.json")
  } yield yet -> Inst.from(inst)).toMap

  // compiled algorithms
  private val funcs: ListBuffer[Func] = ListBuffer()

  // grammar
  private def grammar: Grammar = spec.grammar

  // list of function names which need to replace return step return to resumed step
  // since they have no note step for that return
  private val fixReturnAOs =
    List("GeneratorStart", "AsyncBlockStart", "AsyncGeneratorStart")

  // list of function names which need to replace head to built-in
  // when creating closure (ex: Await)
  private val fixClosurePrefixAOs: List[scala.util.matching.Regex] =
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

  // function builder
  private case class FuncBuilder(
    kind: Func.Kind,
    name: String,
    params: List[Func.Param],
    retTy: IRType,
    body: Step,
    algo: Algorithm,
    returnContext: Option[Ref] = None,
    prefix: Option[Inst] = None,
  ) {
    // get an IR function as the result of compilation of an algorithm
    lazy val result: Func =
      val func = Func(false, kind, name, params, retTy, getBodyInst, Some(algo))
      funcs += func
      func

    // bindings for nonterminals
    var ntBindings: List[(String, Expr, Option[Int])] = algo.head match
      case SyntaxDirectedOperationHead(Some(target), _, _, _, _) =>
        val rhs = grammar.nameMap(target.lhsName).rhsList(target.idx)
        val rhsNames = rhs.nts.map(_.name)
        val rhsBindings = rhsNames.zipWithIndex.map {
          case (name, idx) => (name, ENAME_THIS, Some(idx))
        }
        if (rhsNames contains target.lhsName) rhsBindings
        else (target.lhsName, ENAME_THIS, None) :: rhsBindings
      case _ => List()

    // create a new scope with a given procedure
    def newScope(f: => Unit): Inst = { pushScope; f; popScope }

    // add instructions to the current scope
    def addInst(insts: Inst*): Unit = scopes match
      case current :: rest => current ++= insts
      case _               => error("no current scope")

    // add return to resume instruction
    def addReturnToResume(context: Ref, value: Expr): Unit =
      addInst(
        ICall(newTId, EPop(toStrERef(context, "ReturnCont"), true), List(value)),
      )

    // get next temporal identifier
    def newTId: Temp = Temp(nextTId)

    // get next temporal identifier with expressions
    def newTIdWithExpr: (Temp, Expr) = { val x = newTId; (x, ERef(x)) }

    // get closure name
    def nextCloName: String = s"$name:clo${nextCId}"

    // get body instruction
    private def getBodyInst: Inst =
      (prefix, compileWithScope(this, body)) match
        case (None, i)                  => i
        case (Some(ISeq(ps)), ISeq(bs)) => ISeq(ps ++ bs)
        case (Some(p), ISeq(bs))        => ISeq(p :: bs)
        case (Some(p), b)               => ISeq(List(p, b))

    // push a scope to the scope stack
    private def pushScope: Unit = scopes = ListBuffer() :: scopes

    // pop a scope from the scope stack and produces an instruction
    private def popScope: Inst = scopes match
      case current :: rest => scopes = rest; ISeq(current.toList)
      case _               => error("no current scope")

    // scope stacks
    private var scopes: List[ListBuffer[Inst]] = Nil

    // temporal identifier id counter
    private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
    private var tidCount: Int = 0

    // closure id counter
    private def nextCId: Int = { val cid = cidCount; cidCount += 1; cid }
    private var cidCount: Int = 0
  }
  private type FB = FuncBuilder

  // get function kind
  private def getKind(head: Head): Func.Kind = {
    import Func.Kind.*
    head match {
      case head: AbstractOperationHead       => AbsOp
      case head: NumericMethodHead           => NumMeth
      case head: SyntaxDirectedOperationHead => SynDirOp
      case head: ConcreteMethodHead          => ConcMeth
      case head: InternalMethodHead          => InternalMeth
      case head: BuiltinHead                 => Builtin
    }
  }

  // get function name
  private def getName(head: Head): String = {
    import Func.Kind.*
    head match {
      case head: AbstractOperationHead =>
        head.name
      case head: NumericMethodHead =>
        s"${compile(head.ty)}::${head.name}"
      case head: SyntaxDirectedOperationHead =>
        val Target = SyntaxDirectedOperationHead.Target
        val pre = head.target.fold("<DEFAULT>") {
          case Target(lhsName, idx, subIdx, _) => s"$lhsName[$idx,$subIdx]"
        }
        s"$pre.${head.methodName}"
      case head: ConcreteMethodHead =>
        s"${compile(head.receiverParam.ty)}.${head.methodName}"
      case head: InternalMethodHead =>
        s"${compile(head.receiverParam.ty)}.${head.methodName}"
      case head: BuiltinHead =>
        s"INTRINSICS.${head.ref}"
    }
  }

  // get function parameters
  private def getParams(head: Head): List[Func.Param] = {
    head match {
      case head: AbstractOperationHead =>
        head.params.map(compile)
      case head: NumericMethodHead =>
        head.params.map(compile)
      case head: SyntaxDirectedOperationHead =>
        PARAM_THIS :: head.withParams.map(compile)
      case head: ConcreteMethodHead =>
        compile(head.receiverParam) :: head.params.map(compile)
      case head: InternalMethodHead =>
        compile(head.receiverParam) :: head.params.map(compile)
      case head: BuiltinHead =>
        List(PARAM_THIS, PARAM_ARGS_LIST, PARAM_NEW_TARGET)
    }
  }

  // get prefix instructions for builtin functions
  private def getBuiltinPrefix(ps: List[SParam]): Option[Inst] =
    if (ps.exists(_.kind == SParam.Kind.Ellipsis)) None
    else {
      // add bindings for original arguments
      val argsLen = toStrERef(NAME_ARGS_LIST, "length")
      Some(ISeq(ps.map {
        case SParam(name, SParam.Kind.Variadic, _) =>
          ILet(Name(name), ENAME_ARGS_LIST)
        case SParam(name, _, _) =>
          IIf(
            lessThan(zero, argsLen),
            ILet(Name(name), EPop(ENAME_ARGS_LIST, true)),
            ILet(Name(name), EAbsent),
          )
      }))
    }

  // convert to references
  private inline def toStrERef(base: Ref, props: String*): ERef =
    ERef(toStrRef(base, props*))
  private inline def toStrRef(base: Ref, props: String*): Ref =
    toRef(base, props.map(EStr(_))*)
  private inline def toERef(base: Ref, props: Expr*): ERef =
    ERef(toRef(base, props*))
  private inline def toERef(fb: FB, base: Expr, props: Expr*): ERef =
    ERef(toRef(fb, base, props*))
  private inline def toRef(base: Ref, props: Expr*): Ref =
    props.foldLeft(base)(Prop(_, _))
  private inline def toRef(fb: FB, base: Expr, props: Expr*): Ref =
    toRef(getRef(fb, base), props*)
  private inline def getRef(fb: FB, expr: Expr): Ref = expr match {
    case ERef(ref) => ref
    case _         => val x = fb.newTId; fb.addInst(IAssign(x, expr)); x
  }
  private inline def toIntrinsic(base: Ref, intr: Intrinsic): Prop =
    // convert intr to string for exceptional case in GetPrototypeFromConstructor
    Prop(base, EStr(intr.toString))
  private inline def toEIntrinsic(base: Ref, intr: Intrinsic): ERef =
    toERef(toIntrinsic(base, intr))
  private inline def currentRealm: Ref = toStrRef(GLOBAL_CONTEXT, "Realm")
  private inline def currentIntrinsics: Ref =
    toStrRef(currentRealm, "Intrinsics")

  // compile with a new scope and convert it into an instruction
  private def compileWithScope(fb: FB, step: Step): Inst =
    fb.newScope(compile(fb, step))

  // compile an algorithm to an IR function
  private def compile(algo: Algorithm): Func =
    // TODO consider refactor
    val prefix = algo.head match
      case head: BuiltinHead => getBuiltinPrefix(head.params)
      case _                 => None
    FuncBuilder(
      getKind(algo.head),
      getName(algo.head),
      getParams(algo.head),
      compile(algo.retTy),
      algo.body,
      algo,
      prefix = prefix,
    ).result

  // compile algorithm steps
  private def compile(fb: FB, step: Step): Unit = step match {
    case LetStep(x, expr) =>
      fb.addInst(ILet(compile(x), compile(fb, expr)))
    case SetStep(ref, expr) =>
      fb.addInst(IAssign(compile(fb, ref), compile(fb, expr)))
    case IfStep(cond, thenStep, elseStep) =>
      import CompoundCondition.Op.*
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
      val e = expr.fold(EUndef)(compile(fb, _))
      fb.returnContext match
        case None          => fb.addInst(IReturn(e))
        case Some(context) => fb.addReturnToResume(context, e)
    case AssertStep(cond) =>
      fb.addInst(IAssert(compile(fb, cond)))
    case ForEachStep(ty, x, expr, true, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (length, lengthExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, compile(fb, expr)),
        IAssign(i, EMathVal(0)),
        IAssign(length, toStrERef(list, "length")),
      )
      fb.addInst(
        ILoop(
          "foreach",
          lessThan(iExpr, lengthExpr),
          fb.newScope {
            fb.addInst(ILet(compile(x), toERef(list, iExpr)))
            compile(fb, body)
            fb.addInst(IAssign(i, add(iExpr, EMathVal(1))))
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
        ILoop(
          "foreach",
          lessThan(zero, iExpr),
          fb.newScope {
            fb.addInst(IAssign(i, sub(iExpr, one)))
            fb.addInst(ILet(compile(x), toERef(list, iExpr)))
            compile(fb, body)
          },
        ),
      )
    case ForEachIntegerStep(x, start, cond, ascending, body) =>
      val (i, iExpr) = compileWithExpr(x)
      fb.addInst(ILet(i, compile(fb, start)))
      fb.addInst(
        ILoop(
          "foreach-int",
          compile(fb, cond),
          fb.newScope {
            compile(fb, body)
            val op = if (ascending) add(_, _) else sub(_, _)
            fb.addInst(IAssign(i, op(iExpr, EMathVal(1))))
          },
        ),
      )
    case ForEachArrayIndexStep(x, array, start, ascending, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      val (key, keyExpr) = compileWithExpr(x)
      fb.addInst(
        IAssign(list, EKeys(toStrERef(compile(fb, array), "SubMap"), true)),
        IAssign(i, toStrERef(list, "length")),
        ILoop(
          "foreach-array",
          lessThan(zero, iExpr),
          fb.newScope {
            val cond = and(
              EIsArrayIndex(keyExpr),
              not(lessThan(EConvert(COp.ToNumber, keyExpr), compile(fb, start))),
            )
            fb.addInst(IAssign(i, sub(iExpr, one)))
            fb.addInst(ILet(key, toERef(list, iExpr)))
            fb.addInst(
              IIf(
                cond,
                compileWithScope(fb, body),
                emptyInst,
              ),
            )
          },
        ),
      )
    case ThrowStep(errName) =>
      val proto = Intrinsic(errName, List("prototype"))
      val expr = EMap(
        IRType("OrdinaryObject"),
        List(
          EStr("Prototype") -> toEIntrinsic(currentIntrinsics, proto),
          EStr("ErrorData") -> EUndef,
        ),
      )
      val comp = EComp(ECONST_THROW, expr, ECONST_EMPTY)
      fb.addInst(IReturn(comp))
    case PerformStep(expr) =>
      fb.addInst(IExpr(compile(fb, expr)))
    case AppendStep(expr, ref) =>
      fb.addInst(IPush(compile(fb, expr), ERef(compile(fb, ref)), false))
    case RepeatStep(cond, body) =>
      fb.addInst(
        ILoop(
          "repeat",
          cond.fold(EBool(true))(compile(fb, _)),
          compileWithScope(fb, body),
        ),
      )
    case PushCtxtStep(ref) =>
      fb.addInst(
        IAssign(GLOBAL_CONTEXT, ERef(compile(fb, ref))),
      )
      fb.addInst(IPush(EGLOBAL_CONTEXT, EGLOBAL_EXECUTION_STACK, true))
    case NoteStep(note) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context, false) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context, true) =>
      fb.addInst(IExpr(EPop(EGLOBAL_EXECUTION_STACK, true)))
    case SetEvaluationStateStep(context, paramOpt, body) =>
      val ctxt = compile(fb, context)
      val contName = fb.nextCloName
      val newFb =
        FuncBuilder(
          Func.Kind.Clo,
          contName,
          toParams(paramOpt),
          AnyType,
          body,
          fb.algo,
          if (fixReturnAOs contains fb.name) Some(ctxt) else None,
        )
      newFb.result
      fb.addInst(IAssign(toStrRef(ctxt, "ResumeCont"), ECont(contName)))
    case ResumeEvaluationStep(context, argOpt, paramOpt, steps) =>
      val ctxt = compile(fb, context)
      val returnCont = toStrRef(ctxt, "ReturnCont")
      val (eResumeCont, eReturnCont) =
        (toStrERef(ctxt, "ResumeCont"), ERef(returnCont))
      val contName = fb.nextCloName
      val ps = toParams(paramOpt)
      val bodyStep = BlockStep(StepBlock(steps))
      val newFb =
        FuncBuilder(Func.Kind.Clo, contName, ps, AnyType, bodyStep, fb.algo)
      newFb.result
      fb.addInst(
        IIf(
          isAbsent(eReturnCont),
          IAssign(returnCont, emptyList),
          emptyInst,
        ),
        IPush(ECont(contName), eReturnCont, true),
        ICall(fb.newTId, eResumeCont, argOpt.map(compile(fb, _)).toList),
      )
    case ReturnToResumeStep(context, retStep) =>
      val arg = retStep.expr.fold(EUndef)(compile(fb, _))
      fb.addReturnToResume(compile(fb, context), arg)
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(fb, substep.step)
    case YetStep(yet) =>
      val yetStr = yet.toString(true, false)
      fb.addInst(
        if (manualRules contains yetStr) manualRules(yetStr)
        else IExpr(EYet(yetStr)),
      )
  }

  // compile local variable
  private def compile(x: Variable): Name = Name(x.name)
  private def compileWithExpr(x: Variable): (Name, Expr) =
    val n = Name(x.name); (n, ERef(n))

  // compile references
  private def compile(fb: FB, ref: Reference): Ref = ref match {
    case x: Variable               => compile(x)
    case RunningExecutionContext() => GLOBAL_CONTEXT
    case CurrentRealmRecord()      => toStrRef(GLOBAL_CONTEXT, "Realm")
    case ActiveFunctionObject()    => toStrRef(GLOBAL_CONTEXT, "Function")
    case ref: PropertyReference    => compile(fb, ref)
  }

  private def compile(fb: FB, ref: PropertyReference): Prop =
    val PropertyReference(base, prop) = ref
    val baseRef = compile(fb, base)
    prop match
      case FieldProperty(name)       => Prop(baseRef, EStr(name))
      case ComponentProperty(name)   => Prop(baseRef, EStr(name))
      case IndexProperty(index)      => Prop(baseRef, compile(fb, index))
      case IntrinsicProperty(intr)   => toIntrinsic(baseRef, intr)
      case NonterminalProperty(name) => Prop(baseRef, EStr(name))

  // compile expressions
  private def compile(fb: FB, expr: Expression): Expr = expr match {
    case StringConcatExpression(exprs) =>
      EStrConcat(exprs.map(compile(fb, _)))
    case ListConcatExpression(exprs) =>
      EListConcat(exprs.map(compile(fb, _)))
    case RecordExpression(Type("Completion Record"), fields) =>
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
    case RecordExpression(ty, fields) =>
      val props = fields.map { case (f, e) => compile(fb, f) -> compile(fb, e) }
      EMap(compile(ty), props)
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
        compile(fb, to),
      )
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
    case GetChildrenExpression(nt, expr) =>
      EGetChildren(compile(fb, nt), compile(fb, expr))
    case InvokeAbstractOperationExpression(name, args) =>
      if (simpleOps contains name) simpleOps(name)(args.map(compile(fb, _)))
      else {
        val (x, xExpr) = fb.newTIdWithExpr
        val f = EClo(name, Nil)
        fb.addInst(ICall(x, f, args.map(compile(fb, _))))
        xExpr
      }
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
      val (x, xExpr) = fb.newTIdWithExpr
      val prop @ Prop(base, _) = compile(fb, ref)
      fb.addInst(ICall(x, ERef(prop), ERef(base) :: args.map(compile(fb, _))))
      xExpr
    case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
      val baseExpr = compile(fb, base)
      val callRef = toERef(fb, baseExpr, EStr(name))
      val (x, xExpr) = fb.newTIdWithExpr
      fb.addInst(ICall(x, callRef, baseExpr :: args.map(compile(fb, _))))
      xExpr
    case ReturnIfAbruptExpression(expr, check) =>
      EReturnIfAbrupt(compile(fb, expr), check)
    case ListExpression(entries) =>
      EList(entries.map(compile(fb, _)))
    case yet: YetExpression =>
      EYet(yet.toString(false, false))
    case ReferenceExpression(ref) =>
      ERef(compile(fb, ref))
    case MathOpExpression(op, args) =>
      import MathOpExpression.Op.*
      (op, args) match
        case (Max, _)           => EVariadic(VOp.Max, args.map(compile(fb, _)))
        case (Min, _)           => EVariadic(VOp.Min, args.map(compile(fb, _)))
        case (Abs, List(arg))   => EUnary(UOp.Abs, compile(fb, arg))
        case (Floor, List(arg)) => EUnary(UOp.Floor, compile(fb, arg))
        case (ToBigInt, List(arg)) => EConvert(COp.ToBigInt, compile(fb, arg))
        case (ToNumber, List(arg)) => EConvert(COp.ToNumber, compile(fb, arg))
        case (ToMath, List(arg))   => EConvert(COp.ToMath, compile(fb, arg))
        case _                     => error(s"invalid math operation: $expr")
    case ExponentiationExpression(base, power) =>
      EBinary(BOp.Pow, compile(fb, base), compile(fb, power))
    case BinaryExpression(left, op, right) =>
      EBinary(compile(op), compile(fb, left), compile(fb, right))
    case UnaryExpression(op, expr) =>
      EUnary(compile(op), compile(fb, expr))
    case AbstractClosureExpression(params, captured, body) =>
      val algoName = getName(fb.algo.head)
      val (ck, cn, ps, pre) =
        if (fixClosurePrefixAOs.exists(_.matches(algoName)))
          (
            Func.Kind.Builtin,
            s"INTRINSICS.${fb.nextCloName}",
            List(PARAM_THIS, PARAM_ARGS_LIST, PARAM_NEW_TARGET),
            getBuiltinPrefix(params.map(x => SParam(x.name))),
          )
        else
          (
            Func.Kind.Clo,
            fb.nextCloName,
            params.map(x => Func.Param(compile(x), false, IRType("any"))),
            None,
          )
      val newFb = FuncBuilder(ck, cn, ps, AnyType, body, fb.algo, prefix = pre)
      newFb.result
      EClo(cn, captured.map(compile))
    case XRefExpression(XRefExpression.Op.Algo, id) =>
      EClo(getName(spec.getAlgoById(id).head), Nil)
    case XRefExpression(XRefExpression.Op.ParamLength, id) =>
      EMathVal(spec.getAlgoById(id).head.originalParams.length)
    case XRefExpression(XRefExpression.Op.InternalSlots, id) =>
      // TODO properly handle table column
      EList(for {
        row <- spec.tables(id).rows
        slot = row.head if slot.startsWith("[[") && slot.endsWith("]]")
      } yield EStr(slot.substring(2, slot.length - 2)))
    case SoleElementExpression(list) =>
      toERef(fb, compile(fb, list), EMathVal(0))
    case lit: Literal => compile(fb, lit)
  }

  // compile binary operators
  private def compile(op: BinaryExpression.Op): BOp = op match
    case BinaryExpression.Op.Add => BOp.Plus
    case BinaryExpression.Op.Sub => BOp.Sub
    case BinaryExpression.Op.Mul => BOp.Mul
    case BinaryExpression.Op.Div => BOp.Div
    case BinaryExpression.Op.Mod => BOp.Mod

  // compile unary operators
  private def compile(op: UnaryExpression.Op): UOp = op match
    case UnaryExpression.Op.Neg => UOp.Neg

  // compile literals
  private def compile(fb: FB, lit: Literal): Expr = lit match {
    case ThisLiteral()      => ENAME_THIS
    case NewTargetLiteral() => ENAME_NEW_TARGET
    case HexLiteral(hex, _) => EStr(hex.toChar.toString) // XXX code unit
    case CodeLiteral(code)  => EStr(code)
    case NonterminalLiteral(ordinal, name) =>
      val ntNames = fb.ntBindings.map(_._1)
      // TODO ClassTail[0,3].Contains
      if (ntNames contains name) {
        val xs = fb.ntBindings.filter(_._1 == name)
        xs(ordinal.getOrElse(1) - 1) match
          case (_, base, None)      => base
          case (_, base, Some(idx)) => toERef(fb, base, EMathVal(idx))
      } else EGrammar(name, Nil) // TODO grammar params
    case ConstLiteral(name)                 => EConst(name)
    case StringLiteral(s)                   => EStr(s)
    case FieldLiteral(field)                => EStr(field)
    case SymbolLiteral(sym)                 => toERef(GLOBAL_SYMBOL, EStr(sym))
    case PositiveInfinityMathValueLiteral() => ENumber(Double.PositiveInfinity)
    case NegativeInfinityMathValueLiteral() => ENumber(Double.NegativeInfinity)
    case DecimalMathValueLiteral(n)         => EMathVal(n)
    case NumberLiteral(n)                   => ENumber(n)
    case BigIntLiteral(n)                   => EBigInt(n)
    case TrueLiteral()                      => EBool(true)
    case FalseLiteral()                     => EBool(false)
    case UndefinedLiteral()                 => EUndef
    case NullLiteral()                      => ENull
    case AbsentLiteral()                    => EAbsent
    case UndefinedTypeLiteral()             => EGLOBAL_UNDEF_TYPE
    case NullTypeLiteral()                  => EGLOBAL_NULL_TYPE
    case BooleanTypeLiteral()               => EGLOBAL_BOOL_TYPE
    case StringTypeLiteral()                => EGLOBAL_STRING_TYPE
    case SymbolTypeLiteral()                => EGLOBAL_SYMBOL_TYPE
    case NumberTypeLiteral()                => EGLOBAL_NUMBER_TYPE
    case BigIntTypeLiteral()                => EGLOBAL_BIGINT_TYPE
    case ObjectTypeLiteral()                => EGLOBAL_OBJECT_TYPE
  }

  // compile branch conditions
  private def compile(fb: FB, cond: Condition): Expr = cond match {
    case ExpressionCondition(expr) =>
      compile(fb, expr)
    case InstanceOfCondition(expr, neg, tys) =>
      val (x, xExpr) = fb.newTIdWithExpr
      fb.addInst(IAssign(x, compile(fb, expr)))
      val e = tys.map[Expr](t => ETypeCheck(xExpr, compile(t))).reduce(or(_, _))
      if (neg) not(e) else e
    case HasFieldCondition(ref, neg, field) =>
      val e = isAbsent(toERef(compile(fb, ref), compile(fb, field)))
      if (neg) e else not(e)
    case ProductionCondition(nt, lhsName, rhsName) =>
      val base = compile(fb, nt)
      val prod = grammar.nameMap(lhsName)
      val rhsList = prod.rhsList.zipWithIndex.filter {
        case (rhs, _) => rhs.allNames contains rhsName
      }
      rhsList match
        case (rhs, idx) :: Nil =>
          fb.ntBindings ++= List((rhsName, base, Some(0))) // XXX
          ETypeCheck(base, IRType(lhsName + idx))
        case _ => error("invalid production condition")
    case PredicateCondition(expr, neg, op) =>
      import PredicateCondition.Op.*
      val x = compile(fb, expr)
      val cond = op match {
        case Abrupt =>
          val tv = toERef(fb, x, EStr("Type"))
          and(EIsCompletion(x), not(is(tv, ECONST_NORMAL)))
        case Normal =>
          val tv = toERef(fb, x, EStr("Type"))
          and(EIsCompletion(x), is(tv, ECONST_NORMAL))
        case Finite =>
          not(or(is(x, posInf), is(x, negInf)))
        case Duplicated =>
          EDuplicated(x)
        case Present =>
          not(isAbsent(x))
        case Empty =>
          val lv = toERef(fb, x, EStr("length"))
          is(lv, zero)
        case StrictMode  => T // XXX assume strict mode
        case ArrayIndex  => EIsArrayIndex(x)
        case NonNegative => not(lessThan(x, ENumber(0.0f)))
        case FalseToken  => is(ESourceText(x), EStr("false"))
        case TrueToken   => is(ESourceText(x), EStr("true"))
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
          val accessorFields = List("Get", "Set", "Enumerable", "Configurable")
          or(hasFields(fb, x, dataFields), hasFields(fb, x, accessorFields))
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
      import BinaryCondition.Op.*
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
        case Contains         => EContains(l, r)
        case NContains        => not(EContains(l, r))
      }
    case CompoundCondition(left, op, right) =>
      lazy val l = compile(fb, left)
      lazy val r = compile(fb, right)
      op match
        case CompoundCondition.Op.And   => and(l, r)
        case CompoundCondition.Op.Or    => or(l, r)
        case CompoundCondition.Op.Imply => or(not(l), r)
  }

  // normalize type string
  // TODO refactor
  private def normalizeTy(tname: String): String =
    val trimmed = (if (tname startsWith "a ") tname.drop(2)
                   else if (tname startsWith "an ") tname.drop(3)
                   else tname).replace("-", "").replace("|", "").trim
    trimmed.split(" ").map(_.capitalize).mkString

  // compile types
  private def compile(ty: SType): IRType = IRType(normalizeTy(ty.name))

  // compile algorithm parameters
  private def compile(param: SParam): Func.Param = {
    val SParam(name, skind, stype) = param
    val optional = skind == SParam.Kind.Optional
    Func.Param(Name(name), optional, compile(stype))
  }

  // compile types
  private def compile(ty: Type): IRType =
    if (ty == UnknownType) AnyType else IRType(normalizeTy(ty.name))

  // handle short circuiting
  private def compileShortCircuit(
    fb: FB,
    x: Ref,
    cond: Condition,
  ): Unit =
    val xExpr = toERef(x)
    import CompoundCondition.Op.*
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

  // check if condition contains invoke expression
  private def hasInvokeExpr(cond: Condition): Boolean = {
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

  // instruction helpers
  private inline def toParams(paramOpt: Option[Variable]): List[Func.Param] =
    paramOpt.map(param => Func.Param(Name(param.name))).toList
  private inline def emptyInst = ISeq(List())

  // expression helpers
  private inline def emptyList = EList(List())
  private inline def dataPropClo = EClo("IsDataDescriptor", Nil)
  private inline def accessorPropClo = EClo("IsAccessorDescriptor", Nil)

  // literal helpers
  private val zero = EMathVal(0)
  private val one = EMathVal(1)
  private val posInf = ENumber(Double.PositiveInfinity)
  private val negInf = ENumber(Double.NegativeInfinity)
  private val T = EBool(true)
  private val F = EBool(false)

  // operation helpers
  private inline def isAbsent(expr: Expr) = EBinary(BOp.Eq, expr, EAbsent)
  private def not(expr: Expr) = expr match
    case EBool(b)              => EBool(!b)
    case EUnary(UOp.Not, expr) => expr
    case _                     => EUnary(UOp.Not, expr)
  private inline def lessThan(l: Expr, r: Expr) = EBinary(BOp.Lt, l, r)
  private inline def add(l: Expr, r: Expr) = EBinary(BOp.Plus, l, r)
  private inline def sub(l: Expr, r: Expr) = EBinary(BOp.Sub, l, r)
  private inline def and(l: Expr, r: Expr) = EBinary(BOp.And, l, r)
  private inline def or(l: Expr, r: Expr) = EBinary(BOp.Or, l, r)
  private inline def is(l: Expr, r: Expr) = EBinary(BOp.Eq, l, r)
  private inline def hasFields(
    fb: FuncBuilder,
    base: Expr,
    fs: List[String],
  ): Expr =
    val conds = fs.map(f => isAbsent(toERef(fb, base, EStr(f))))
    not(conds.reduce { case (a, b) => or(a, b) })

  // simple operations
  private type SimpleOp = PartialFunction[List[Expr], Expr]
  private def arityCheck(pair: (String, SimpleOp)): (String, SimpleOp) = {
    val (name, f) = pair
    name -> (args =>
      optional(f(args)).getOrElse(
        error(s"invalid arguments: $name(${args.mkString(", ")})"),
      ),
    )
  }
  private val simpleOps: Map[String, SimpleOp] = Map(
    arityCheck("ParseText" -> { case List(code, rule) => EParse(code, rule) }),
    arityCheck("Type" -> { case List(expr) => ETypeOf(expr) }),
    arityCheck("Completion" -> { case List(expr) => expr }),
    arityCheck("ReturnIfAbrupt" -> {
      case List(expr) => EReturnIfAbrupt(expr, true)
    }),
  )
}
object Compiler {
  def apply(spec: Spec): Program = new Compiler(spec).result
}
