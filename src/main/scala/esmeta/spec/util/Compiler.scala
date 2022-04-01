package esmeta.spec.util

import esmeta.MANUALS_DIR
import esmeta.ir.{Type => IRType, *}
import esmeta.ir.util.{Walker => IRWalker}
import esmeta.lang.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker}
import esmeta.spec.{Param => SParam, Type => SType, *}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{ListBuffer, Stack}

/** Compiler from metalangauge to IR */
class Compiler(val spec: Spec) {

  /** compiled specification */
  def result: Program = {
    // load manually created AOs
    for (file <- walkTree(MANUALS_DIR) if irFilter(file.getName))
      funcs += Func.fromFile(file.toString)
    val manualNames = funcs.map(_.name)

    // compile algorithms in spec
    for {
      algo <- spec.algorithms
      name = algo.head.fname
      if !(excluded.contains(name) || manualNames.contains(name))
    } compile(algo)

    // result
    val program = Program(funcs.toList)

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

  // list of shorthands
  // NOTE: https://github.com/tc39/ecma262/issues/2384
  private val shorthands = List(
    "IfAbruptCloseIterator",
    "IfAbruptRejectPromise",
  )

  // list of not-compiled function names
  private val excluded =
    "INTRINSICS.Array.prototype[@@unscopables]" :: shorthands

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
    def newScope(f: => Unit): Inst = {
      scopes.push(ListBuffer()); f; ISeq(scopes.pop.toList)
    }

    // set backward egde from ir to lang
    def withLang[T](lang: Syntax)(f: => T): T = {
      langs.push(lang); val result = f; langs.pop
      result
    }

    // add instructions to the current scope
    def addInst(insts: Inst*): Unit = scopes.head ++= insts
      .flatMap {
        case ISeq(is) => is
        case i        => List(i)
      }
      .map(backEdgeWalker(this).walk(_))

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

    // get continuation name
    def nextContName: String = s"$name:cont${nextCId}"

    // get body instruction
    private def getBodyInst: Inst =
      (prefix, compileWithScope(this, body)) match
        case (None, i)                  => i
        case (Some(ISeq(ps)), ISeq(bs)) => ISeq(ps ++ bs)
        case (Some(p), ISeq(bs))        => ISeq(p :: bs)
        case (Some(p), b)               => ISeq(List(p, b))

    // scope stacks
    private var scopes: Stack[ListBuffer[Inst]] = Stack()

    // lang stacks
    val langs: Stack[Syntax] = Stack()

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
  // TODO consider refactor
  private def compile(algo: Algorithm): Func =
    val prefix = algo.head match
      case head: BuiltinHead => getBuiltinPrefix(head.params)
      case _                 => None
    FuncBuilder(
      getKind(algo.head),
      algo.head.fname,
      algo.head.funcParams.map(compile),
      compile(algo.retTy),
      algo.body,
      algo,
      prefix = prefix,
    ).result

  // compile algorithm steps
  private def compile(fb: FB, step: Step): Unit = fb.withLang(step)(step match {
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
      val (list, listExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, compile(fb, expr)),
        IAssign(i, zero),
      )
      fb.addInst(
        ILoop(
          "foreach",
          lessThan(iExpr, toStrERef(list, "length")),
          fb.newScope {
            fb.addInst(ILet(compile(x), toERef(list, iExpr)))
            compile(fb, body)
            fb.addInst(IAssign(i, add(iExpr, one)))
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
            fb.addInst(IAssign(i, op(iExpr, one)))
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
    case ForEachParseNodeStep(x, expr, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      val (length, lengthExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, toERef(fb, compile(fb, expr), EStr("children"))),
        IAssign(i, zero),
        IAssign(length, toStrERef(list, "length")),
      )
      fb.addInst(
        ILoop(
          "foreach-node",
          lessThan(iExpr, lengthExpr),
          fb.newScope {
            fb.addInst(ILet(compile(x), toERef(list, iExpr)))
            compile(fb, body)
            fb.addInst(IAssign(i, add(iExpr, one)))
          },
        ),
      )
    case ThrowStep(expr) =>
      val comp = EComp(ECONST_THROW, compile(fb, expr), ECONST_EMPTY)
      fb.addInst(IReturn(comp))
    case PerformStep(expr) =>
      compile(fb, expr) match
        case era: EReturnIfAbrupt => fb.addInst(IExpr(era))
        case _                    =>
    case PerformBlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(fb, substep.step)
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
      fb.addInst(IPush(ERef(compile(fb, ref)), EGLOBAL_EXECUTION_STACK, true))
    case NoteStep(note) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context, false) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context, true) =>
      fb.addInst(IExpr(EPop(EGLOBAL_EXECUTION_STACK, true)))
    case SetEvaluationStateStep(context, paramOpt, body) =>
      val ctxt = compile(fb, context)
      val contName = fb.nextContName
      val newFb =
        FuncBuilder(
          Func.Kind.Cont,
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
      val contName = fb.nextContName
      val ps = toParams(paramOpt)
      val bodyStep = BlockStep(StepBlock(steps))
      val newFb =
        FuncBuilder(Func.Kind.Cont, contName, ps, AnyType, bodyStep, fb.algo)
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
      val inst = manualRules.get(yetStr).getOrElse(IExpr(EYet(yetStr)))
      fb.addInst(backEdgeWalker(fb, force = true).walk(inst))
  })

  // compile local variable
  private def compile(x: Variable): Name = Name(x.name)
  private def compileWithExpr(x: Variable): (Name, Expr) =
    val n = Name(x.name); (n, ERef(n))

  // compile references
  private def compile(fb: FB, ref: Reference): Ref = ref match {
    case x: Variable               => compile(x)
    case RunningExecutionContext() => GLOBAL_CONTEXT
    case SecondExecutionContext()  => toRef(GLOBAL_EXECUTION_STACK, EMathVal(1))
    case CurrentRealmRecord()      => currentRealm
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
  private def compile(fb: FB, expr: Expression): Expr =
    fb.withLang(expr)(expr match {
      case StringConcatExpression(exprs) =>
        EVariadic(VOp.Concat, exprs.map(compile(fb, _)))
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
        val props = fields.map {
          case (f, e) => compile(fb, f) -> compile(fb, e)
        }
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
          case (Max, _)         => EVariadic(VOp.Max, args.map(compile(fb, _)))
          case (Min, _)         => EVariadic(VOp.Min, args.map(compile(fb, _)))
          case (Abs, List(arg)) => EUnary(UOp.Abs, compile(fb, arg))
          case (Floor, List(arg))    => EUnary(UOp.Floor, compile(fb, arg))
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
        val algoName = fb.algo.head.fname
        val (ck, cn, ps, pre) =
          if (fixClosurePrefixAOs.exists(_.matches(algoName)))
            (
              Func.Kind.BuiltinClo,
              fb.nextCloName,
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
        val newFb =
          FuncBuilder(ck, cn, ps, AnyType, body, fb.algo, prefix = pre)
        newFb.result
        EClo(cn, captured.map(compile))
      case XRefExpression(XRefExpression.Op.Algo, id) =>
        EClo(spec.getAlgoById(id).head.fname, Nil)
      case XRefExpression(XRefExpression.Op.ParamLength, id) =>
        EMathVal(spec.getAlgoById(id).head.originalParams.length)
      case XRefExpression(XRefExpression.Op.InternalSlots, id) =>
        // TODO properly handle table column
        EList(for {
          row <- spec.tables(id).rows
          slot = row.head if slot.startsWith("[[") && slot.endsWith("]]")
        } yield EStr(slot.substring(2, slot.length - 2)))
      case SoleElementExpression(list) =>
        toERef(fb, compile(fb, list), zero)
      case lit: Literal => compile(fb, lit)
    })

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
    case HexLiteral(hex, _) => ECodeUnit(hex.toChar)
    case CodeLiteral(code)  => EStr(code)
    case NonterminalLiteral(ordinal, name, flags) =>
      val ntNames = fb.ntBindings.map(_._1)
      // TODO ClassTail[0,3].Contains
      if (ntNames contains name) {
        val xs = fb.ntBindings.filter(_._1 == name)
        xs(ordinal.getOrElse(1) - 1) match
          case (_, base, None)      => base
          case (_, base, Some(idx)) => toERef(fb, base, EMathVal(idx))
      } else EGrammar(name, flags.map(_ startsWith "+"))
    case ConstLiteral(name)                  => EConst(name)
    case StringLiteral(s)                    => EStr(s)
    case FieldLiteral(field)                 => EStr(field)
    case SymbolLiteral(sym)                  => toERef(GLOBAL_SYMBOL, EStr(sym))
    case ProductionLiteral(lhsName, rhsName) =>
      // XXX need to handle arguments, children?
      val (lhs, rhsIdx) = getProductionData(lhsName, rhsName)
      ESyntactic(lhsName, lhs.params.map(_ => true), rhsIdx, Nil)
    case ErrorObjectLiteral(name) =>
      val proto = Intrinsic(name, List("prototype"))
      EMap(
        IRType("OrdinaryObject"),
        List(
          EStr("Prototype") -> toEIntrinsic(currentIntrinsics, proto),
          EStr("ErrorData") -> EUndef,
        ),
      )
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
  private def compile(fb: FB, cond: Condition): Expr =
    fb.withLang(cond)(cond match {
      case ExpressionCondition(expr) =>
        compile(fb, expr)
      case InstanceOfCondition(expr, neg, tys) =>
        val (x, xExpr) = fb.newTIdWithExpr
        fb.addInst(IAssign(x, compile(fb, expr)))
        val e =
          tys.map[Expr](t => ETypeCheck(xExpr, compile(t))).reduce(or(_, _))
        if (neg) not(e) else e
      case HasFieldCondition(ref, neg, field) =>
        val e = isAbsent(toERef(compile(fb, ref), compile(fb, field)))
        if (neg) e else not(e)
      // XXX need to be generalized?
      case ProductionCondition(nt, lhsName, rhsName) =>
        val base = compile(fb, nt)
        val (_, rhsIdx) = getProductionData(lhsName, rhsName)
        fb.ntBindings ++= List((rhsName, base, Some(0)))
        ETypeCheck(base, IRType(lhsName + rhsIdx))
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
          case NonNegative =>
            and(not(lessThan(x, ENumber(0.0f))), isIntegral(x))
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
            ETypeCheck(x, IRType("Nonterminal"))
          case IntegralNumber => isIntegral(x)
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
    })

  // compile algorithm parameters
  private def compile(param: SParam): Func.Param = {
    val SParam(name, skind, stype) = param
    val optional = skind == SParam.Kind.Optional
    Func.Param(Name(name), optional, compile(stype))
  }

  // compile types
  private def compile(ty: SType): IRType = compile(ty.toLang)
  private def compile(ty: Type): IRType =
    if (ty == UnknownType) AnyType else IRType(ty.normalized.name)

  // compile shorthands
  // NOTE: arguments for shorthands are named local identifiers
  private def compileShorthand(fb: FB, fname: String, args: List[Expr]): Expr =
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
    fb.addInst(backEdgeWalker(fb, force = true).walk(renamed))
    EUndef // NOTE: unused expression

  // handle short circuiting
  private def compileShortCircuit(
    fb: FB,
    x: Ref,
    cond: Condition,
  ): Unit = fb.withLang(cond) {
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
  }

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

  // production helpers
  private def getProductionData(lhsName: String, rhsName: String): (Lhs, Int) =
    val prod = grammar.nameMap(lhsName)
    val rhsList = prod.rhsList.zipWithIndex.filter {
      case (rhs, _) if rhsName == "[empty]" => rhs.isEmpty
      case (rhs, _)                         => rhs.allNames contains rhsName
    }
    rhsList match
      case (rhs, idx) :: Nil => (prod.lhs, idx)
      case _                 => error("invalid production")

  // walker for adjusting backward edge from ir to lang
  private def backEdgeWalker(fb: FB, force: Boolean = false) = new IRWalker {
    override def walk(i: Inst) =
      if (force || i.langOpt.isEmpty) i.setLangOpt(fb.langs.headOption)
      super.walk(i)
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
  private val zero = EMathVal(BigDecimal.exact(0))
  private val one = EMathVal(BigDecimal.exact(1))
  private val posInf = ENumber(Double.PositiveInfinity)
  private val negInf = ENumber(Double.NegativeInfinity)
  private val T = EBool(true)
  private val F = EBool(false)

  // operation helpers
  private inline def isAbsent(x: Expr) = EBinary(BOp.Eq, x, EAbsent)
  private inline def isIntegral(x: Expr) =
    val m = EConvert(COp.ToMath, x)
    and(ETypeCheck(x, IRType("Number")), is(m, floor(m)))
  private def not(expr: Expr) = expr match
    case EBool(b)              => EBool(!b)
    case EUnary(UOp.Not, expr) => expr
    case _                     => EUnary(UOp.Not, expr)
  private inline def floor(expr: Expr) = EUnary(UOp.Floor, expr)
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
