package esmeta.es.util

import esmeta.es.*
import esmeta.lang.*
import esmeta.lang.util.{UnitWalker as LangUnitWalker}
import esmeta.spec.*
import esmeta.spec.BuiltinPath.YetPath
import esmeta.util.BaseUtils.*
import esmeta.util.ManualInfo

import scala.annotation.tailrec
import scala.collection.mutable

/** polyfill generator */
object PolyfillGenerator {
  def apply(spec: Spec, dslDir: Option[String]): List[Polyfill] =
    new PolyfillGenerator(spec, dslDir).result

  val targetPatterns = List(
    // https://tc39.es/ecma262/#sec-properties-of-the-string-prototype-object
    """INTRINSICS\.(get:|set:)?String(\..*)?""",
    // https://tc39.es/ecma262/#sec-properties-of-the-array-prototype-object
    """INTRINSICS\.(get:|set:)?Array(\..*)?""",
    // https://tc39.es/ecma262/#sec-map-objects
    """INTRINSICS\.(get:|set:)?Map.*""",
    // https://tc39.es/ecma262/#sec-set-objects
    """INTRINSICS\.(get:|set:)?Set.*""",
    """INTRINSICS\.JSON\.stringify""",
    // https://tc39.es/ecma262/#sec-iterator-objects
    // """INTRINSICS\.(get:|set:)?Iterator.*""",
    // https://tc39.es/ecma262/#sec-promise-objects
    """INTRINSICS\.(get:|set:)?Promise.*""",
    """INTRINSICS\.(get:|set:)?WeakMap.*""",
    """INTRINSICS\.(get:|set:)?WeakSet.*""",
    """CanonicalNumericIndexString""",
    """INTRINSICS\.Math\.(floor|round|ceil|abs|trunc|max|min|sign)""",
    // no RegExp targets - we model them in manual way
  )

  /** Intrinsic names (`INTRINSICS.<name>`) whose builtin object declares a
    * [[Construct]] internal method in the manual intrinsics model — the same
    * signal `Initialize.intrTypes` uses (`obj.map contains "Construct"`), read
    * straight from `manuals/intrinsics` so it stays authoritative without
    * needing a CFG. A generated polyfill matching one of these is stamped
    * CONSTRUCTABLE so the runtime permits `new`; everything else defaults to
    * non-constructable. (Note: the manual model gives Symbol/BigInt a throwing
    * [[Construct]], so they are included here too — matching esmeta's
    * semantics, not native IsConstructor.)
    */
  lazy val constructorTargets: Set[String] =
    ManualInfo.intrinsics.replacedModels.iterator
      .filter(_.imap.exists(_._1 == "Construct"))
      .map(m => s"INTRINSICS.${m.name}")
      .toSet

  val ignoreTargets = List(
    // Unsupported
    "INTRINSICS.MapIteratorPrototype.next",
    "INTRINSICS.SetIteratorPrototype.next",

    // Yet AOs
    "StringCreate",
    "ArrayCreate",
    "ArraySpeciesCreate",
    "AsyncGeneratorYield",
    "Await",
    "CreateBuiltinFunction",
    "CreateIteratorFromClosure",
    "GeneratorResume",
    "GeneratorStart",
    "GeneratorYield",
    "GetFunctionRealm",
    "GetPrototypeFromConstructor",
    "RegExpInitialize",
    "StringToNumber",
    "StringToBigInt",

    // Generator
    "GeneratorYield",
    "GeneratorStart",
    "GeneratorValidate",
    "GeneratorResume",
    "GeneratorResumeAbrupt",
    "CreateIteratorFromClosure",
    "CreateArrayIterator",
  )

  /** boxed-primitive internal slots → the constructor a raw boxed value is an
    * `instanceof`. These slots aren't represented in the object model, so a
    * spec "_x_ has a [[…Data]] internal slot" check is approximated on the raw
    * value.
    */
  val boxedSlotCtor = Map(
    "StringData" -> "String",
    "NumberData" -> "Number",
    "BooleanData" -> "Boolean",
    "BigIntData" -> "BigInt",
    "SymbolData" -> "Symbol",
  )
}

/** extensible helper of polyfill generator */
class PolyfillGenerator(spec: Spec, dslDir: Option[String]) {

  import Polyfill.*, PolyfillGenerator.*

  /** generated polyfills */
  lazy val result: List[Polyfill] =
    val optimizedTargets = optPaths.foldLeft(targets) { (x, optim) => optim(x) }
    for { algo <- optimizedTargets } yield compile(algo)

  /** list of optimization paths */
  val optPaths: List[TransformPath] =
    List(ShorthandInlinePath(spec)) ++ (dslDir.map(dsl.DSLPath(_))) ++ List(
      CompletionPath(),
    )

  /** list of polyfill targets composed recursively from targetPattern */
  lazy val targets: List[Algorithm] = {
    @tailrec
    def expand[T](acc: Set[T], curr: Set[T])(
      f: Set[T] => Set[T],
    ): Set[T] = {
      if (curr.isEmpty) acc
      else
        val next = f(curr) -- acc
        expand(acc ++ next, next)(f)
    }

    def getAOCallees(algo: Algorithm): Set[Algorithm] = {
      val result = mutable.Set[Algorithm]()
      new LangUnitWalker {
        override def walk(expr: Expression): Unit = expr match
          case InvokeAbstractOperationExpression(name, args, _) =>
            result ++= spec.fnameMap.get(name)
            walkList(args, walk)
          // Numeric methods (e.g. `Number::equal`) are real algorithms too; pull
          // them into the worklist so a polyfill file is generated and imported,
          // instead of leaving a dangling `Number__equal` reference.
          case InvokeNumericMethodExpression(base, name, args) =>
            result ++= spec.fnameMap.get(s"$base::$name")
            walkList(args, walk)
          case XRefExpression(
                XRefExpressionOperator.Algo |
                XRefExpressionOperator.Definition |
                XRefExpressionOperator.InternalMethod,
                id,
              ) =>
            println(id)
            val targetAlgo = spec.getAlgoById(id)
            // Elem is reference; It is deinitialized at Inspector stage;;;;;;;
            val capturedAlgo = targetAlgo.copy(head = targetAlgo.head match {
              case a @ BuiltinHead(YetPath(_), _, _) => a.copy(YetPath(id))
              case x                                 => x
            })
            result += capturedAlgo
          case _ => super.walk(expr)
      }.walk(algo.body)
      result.toSet
    }

    // Initial targets filtered by `targetPatterns`
    val initialTargets = spec.algorithms
      .filter(algo => targetPatterns.exists(algo.name.matches))
      .toSet

    // Build maximum set based on worklist algorithm
    val result = expand(initialTargets, initialTargets) {
      _.flatMap(getAOCallees)
    }

    // Filter out `ignoreTargets` & Sort the result
    result
      .filter(algo => !ignoreTargets.contains(algo.name))
      .toList
      .sortWith(_.name < _.name)
  }

  private val IS_PRESENT = "IsPresent"
  private val AO_HEADER = "AO";
  private val INTERNAL_HEADER = s"${RUNTIME}.IN";
  private val RESERVED_WORDS = Set("return")

  /** compile an algorithm into a polyfill */
  def compile(algo: Algorithm): Polyfill =
    val pb = PolyfillBuilder()

    val name = algo.name
    val isConstructor = constructorTargets.contains(name)
    val params = algo.head.originalParams
    val prelude = compilePrelude(pb, algo.head, algo.body)
    val body =
      try {
        // TODO remove this catch after implementing all steps
        compileWithScope(pb, algo.body)
      } catch {
        case e: Throwable =>
          println("-" * 80)
          println(algo)
          println("-" * 80)
          println(pb.currentResult)
          println("-" * 80)
          throw e
      }
    val hasThis = algo.head match
      case _: BuiltinHead => true
      case _              => false
    val isAbstractOp = algo.head.isInstanceOf[AbstractOperationHead]
    // AOs referenced by this polyfill are imported as `AO__<name>` from their files.
    val aoImports = {
      val names = mutable.Set[String]()
      new LangUnitWalker {
        override def walk(expr: Expression): Unit = expr match
          case InvokeAbstractOperationExpression(n, args, _) =>
            names += n; walkList(args, walk)
          case _ => super.walk(expr)
      }.walk(algo.body)
      names.filterNot(_ == name).toList.sorted
    }
    // Numeric methods referenced by this polyfill are imported as `<ty>__<name>`
    // from their own generated files (mirrors aoImports above).
    val numericImports = {
      val names = mutable.Set[String]()
      new LangUnitWalker {
        override def walk(expr: Expression): Unit = expr match
          case InvokeNumericMethodExpression(base, n, args) =>
            names += s"${base}__$n"; walkList(args, walk)
          case _ => super.walk(expr)
      }.walk(algo.body)
      names.toList.sorted
    }
    Polyfill(
      name,
      params,
      prelude ++ body,
      hasThis = hasThis,
      isAbstractOp = isAbstractOp,
      aoImports = aoImports,
      numericImports = numericImports,
      isConstructor = isConstructor,
    )

  def compilePrelude(pb: PolyfillBuilder, head: Head, body: Step): Stmt =
    pb.newScope({
      val existenceCheckVariables = {
        var result = mutable.Set[String]()
        new LangUnitWalker {
          override def walk(cond: Condition): Unit =
            import PredicateConditionOperator.*
            cond match
              case PredicateCondition(
                    ReferenceExpression(Variable(name, _, _, _)),
                    _,
                    Present,
                  ) =>
                result += name
              case _ =>
        }.walk(body)
        result.toSet
      }

      // Runtime argument offset: `$` (RUNTIME) is always argument 0, and a
      // BuiltinHead also receives `$this` at argument 1, so the spec parameter
      // at originalParams index `i` is the JS `arguments[i + argOffset]`.
      val argOffset = 1 + (head match { case _: BuiltinHead => 1; case _ => 0 })

      head.originalParams.zipWithIndex.foreach((param, index) => {
        if (existenceCheckVariables.contains(param.name))
          pb.addStmt(
            NormalStmt(
              s"var ${param.name}$IS_PRESENT = arguments.length > ${index + argOffset};",
            ),
          )
      })

      // Optional parameters are already rendered as `name?` in the signature
      // (see Polyfill.headToString), so they default to `undefined` when the
      // argument is absent — no initializer needed. The previous `var name =
      // arguments[index]` redeclaration both shadowed the typed parameter with
      // `any` (TS2403) and indexed `arguments` without the receiver/runtime
      // offset (so it actually read `$`/`$this`), so it is dropped entirely.
    })

  /** compile with a new scope and convert it into a statement */
  def compileWithScope(pb: PolyfillBuilder, step: Step): Stmt =
    pb.newScope(compile(pb, step))

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
    case InvokeShorthandStep(name, args) => ???
    case AppendStep(expr, ref) =>
      pb.addStmt(
        NormalStmt(
          s"${RUNTIME}.append(${compile(pb, ref)}, ${compile(pb, expr)})",
        ),
      )
    case InsertStep(expr, ref) => ???
    case PrependStep(expr, ref) =>
      pb.addStmt(
        NormalStmt(
          s"${RUNTIME}.prepend(${compile(pb, ref)}, ${compile(pb, expr)})",
        ),
      )
    case AddStep(expr, ref) => ???
    case ReplaceStep(oldElem, newElem, ref) =>
      pb.addStmt(
        NormalStmt(
          s"${INTERNAL_HEADER}__Replace(${compile(pb, ref)}, ${compile(pb, oldElem)}, ${compile(pb, newElem)})",
        ),
      )
    case RemoveStep(t, p, l) =>
      t match {
        case RemoveStep.Target.First(None) =>
          pb.addStmt(NormalStmt(s"${compile(pb, l)}.shift()"))
        case RemoveStep.Target.Last(None) =>
          pb.addStmt(NormalStmt(s"${compile(pb, l)}.pop()"))
        case _ => ???
      }
    case PushContextStep(ref)       => ???
    case SuspendStep(ref, rm)       => {}
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
      // `for...of` over the compiled iterable — arrays are iterable, and an
      // `IntRange` source compiles to the lazy `$.range` iterable, so the same
      // generic loop drives both (the element binding comes from `for...of`, no
      // indexed access needed).
      val compiledExpr = compile(pb, expr)
      val element = compile(elem)
      val compiledBody = compileWithScope(pb, body)
      pb.addStmt(ForOfStmt(element, compiledExpr, compiledBody))
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
          newBranchId,
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
        case ThrowStep(name) => pb.addStmt(NormalStmt(s"throw $name;"))
        case x               => compile(pb, x)
      }
    case MetaStep(name, multiline, _) => ???
  }

  /** compile local variable */
  def compile(x: Variable): String =
    if (RESERVED_WORDS.contains(x.name))
      s"${x.name}_var"
    else
      x.name

  /** compile references */
  def compile(pb: PolyfillBuilder, ref: Reference): String = ref match {
    case x: Variable => compile(x)
    case Access(base, name, kind, _)
        if kind == AccessKind.Field && boxedSlotCtor.contains(name) =>
      // boxed-primitive data slot ([[BooleanData]]/[[BigIntData]]/…): not in the
      // object model, so read the underlying primitive off the raw boxed value
      // (the guarding HasField check is the matching `instanceof`).
      val b = compile(pb, base)
      s"${RUNTIME}.default(${RUNTIME}.value($b as Lifted<${boxedSlotCtor(name)}>).valueOf(), [$b])"
    case Access(base, name, kind, _) =>
      s"${compile(pb, base)}[\"$name\" ${if kind == AccessKind.Field then "/* TODO INTERNAL : internal access */"
      else ""}]"
    case ValueOf(base)              => compile(pb, base)
    case IntrinsicField(base, intr) => ???
    case IndexLookup(base, index) =>
      s"${compile(pb, base)}[${compile(pb, index)}]"
    case BindingLookup(base, binding)   => ???
    case NonterminalLookup(base, nt)    => ???
    case PositionalElement(base, true)  => s"${compile(pb, base)}[0]"
    case PositionalElement(base, false) => ???
    case IntrinsicObject(base, expr)    => ???
    case RunningExecutionContext() =>
      "this" // ??? TODO Single-Runtime Assumption
    case SecondExecutionContext() => ???
    case CurrentRealmRecord()     => "globalThis"
    case ActiveFunctionObject()   => "_self"
    case AgentRecord()            => ???
    case MetaReference(name, _)   => ???
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
        .reduceLeft((acc, p) => s"${RUNTIME}.concatenate($acc, $p)")
    case ListConcatExpression(es) =>
      s"[].concat(${es.map(compile(pb, _)).mkString(", ")})"
    case ListCopyExpression(expr) => s"${compile(pb, expr)}.slice()"
    case RecordExpression(rawName, fields, form) =>
      s"{${fields.map((fieldLit, fieldExpr) => s"\"${fieldLit.name}\": ${compile(pb, fieldExpr)}").mkString(", ")}}"
    case LengthExpression(ReferenceExpression(ref)) =>
      s"${RUNTIME}.length(${compile(pb, ref)})"
    case LengthExpression(expr)              => ???
    case StringExpression(expr)              => compile(pb, expr)
    case SubstringExpression(expr, from, to) =>
      // An omitted `to` means "to the end of the string"; emit its length so the
      // 3-arg runtime `substring(s, from, to)` always gets a concrete end index.
      // from/to are spec-typed integers — cast (trust the frontend, as with AO
      // args) so a value the spec proves numeric still types after equality lost
      // its narrowing.
      val base = compile(pb, expr)
      val end = to.fold(s"${RUNTIME}.length($base)")(t =>
        s"(${compile(pb, t)} as Lifted<number>)",
      )
      s"${RUNTIME}.substring($base, (${compile(pb, from)} as Lifted<number>), $end)"
    case TrimExpression(expr, leading, trailing) =>
      s"${RUNTIME}.trim(${compile(pb, expr)}, $leading, $trailing)"
    case NumberOfExpression(_, _, ReferenceExpression(ref), _) =>
      // a List's length is a value too — wrap it so it flows through the ops.
      s"${RUNTIME}.default<number>(${compile(pb, ref)}.length, [])"
    case NumberOfExpression(_, _, expr, _) => ???
    case IntrinsicExpression(intr)         =>
      // An intrinsic referenced as a *value* (e.g. `%Object.prototype.toString%`)
      // must enter the model lifted, so it flows like any other Lifted value
      // (assigned into a Lifted var, passed to AO__Call, …). Direct-call callees
      // go through the Reference path (IntrinsicField/IntrinsicObject), not here.
      val raw =
        if (intr.props.isEmpty) s"${intr.base}"
        else s"${intr.base}.${intr.props.mkString(".")}"
      s"${RUNTIME}.default($raw as Function as Unlifted<Function>, [])"
    case SourceTextExpression(expr)      => ???
    case CoveredByExpression(code, rule) => ???
    case GetItemsExpression(nt, expr @ NonterminalLiteral(_, _, _, _)) =>
      ???
    case expr: GetItemsExpression                           => ???
    case InvokeAbstractOperationExpression(name, args, tag) =>
      // Cast each argument to the callee's declared parameter type. AO calls are
      // spec-typed contracts, so this is a "trust the frontend" cast that closes
      // TS control-flow-narrowing gaps (e.g. a value the spec proves is a String
      // but TS still sees as Lifted<unknown> across correlated conditions).
      val params =
        spec.fnameMap.get(name).map(_.head.originalParams).getOrElse(Nil)
      val argStrs = args.zipWithIndex.map { (arg, i) =>
        val c = compile(pb, arg)
        params.lift(i).fold(c)(p => s"($c as ${Polyfill.tsParamType(p.ty)})")
      }
      s"${AO_HEADER}__$name(${(RUNTIME :: argStrs).mkString(", ")})"
    case InvokeNumericMethodExpression(ty, name, args) =>
      // A numeric method (`Number::equal`, …) is generated as a sibling polyfill
      // `<ty>__<name>` taking `$` first; call it like an AO — runtime receiver
      // plus args cast to the callee's declared param type past lost narrowing.
      val params =
        spec.fnameMap
          .get(s"$ty::$name")
          .map(_.head.originalParams)
          .getOrElse(Nil)
      val argStrs = args.zipWithIndex.map { (arg, i) =>
        val c = compile(pb, arg)
        params.lift(i).fold(c)(p => s"($c as ${Polyfill.tsParamType(p.ty)})")
      }
      s"${ty}__$name(${(RUNTIME :: argStrs).mkString(", ")})"
    case InvokeAbstractClosureExpression(ref, args) =>
      s"${compile(pb, ref)}(${args.map(compile(pb, _)).mkString(", ")})"
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
        case LiteralSyntax(entries) => s"[${compile(pb, entries)}]"
        case SoleElement(entry)     => s"[${compile(pb, entry)}]"
        // `Lifted<never>[]` (= never[]) is assignable to any list param, and
        // `$.append` still pins its element type from the pushed value.
        case EmptyList(isNewUsed, typeDesc) => "[] as Lifted<never>[]"
        case IntRange(
              from,
              isFromInclusive,
              to,
              isToInclusive,
              isAscending,
            ) =>
          // The "integers in the interval" notation is just an integer-loop index
          // sequence, so it compiles to the same lazy `range` op a ForEachInteger
          // step uses (then driven by a `for...of`). Gets its own branch id so its
          // loop bound stays a flippable path constraint when `to` is symbolic.
          s"${RUNTIME}.range(${compile(pb, from)}, $isFromInclusive, ${compile(pb, to)}, $isToInclusive, $isAscending, Number.MAX_SAFE_INTEGER - $newBranchId)"
    case YetExpression(str, block) =>
      // Manual 1:1 override (see manuals/polyfill-rule.json). Both expression-
      // position YETs and statement-position ones (via YetStep) funnel here, so
      // the looked-up snippet must be valid wherever this `str` appears.
      ManualInfo.polyfillRule.getOrElse(
        str,
        s"throw new Error(\"YET: ${str.replace("\"", "\\\"")}\")",
      )
    case ReferenceExpression(ref) => compile(pb, ref)
    case MathFuncExpression(op, args) =>
      s"${RUNTIME}.${compile(op)}(${compile(pb, args)})"
    case ConversionExpression(op, expr, form) => compile(pb, expr)
    case ExponentiationExpression(base, power) =>
      s"${RUNTIME}.exponentiate(${compile(pb, base)}, ${compile(pb, power)})"
    case BinaryExpression(left, op, right) =>
      // numeric operands — cast (as with AO args) past equality's lost narrowing.
      s"${RUNTIME}.${compile(op)}((${compile(pb, left)} as Lifted<number>), (${compile(pb, right)} as Lifted<number>))"
    case UnaryExpression(op, expr) =>
      s"${RUNTIME}.${compile(op)}((${compile(pb, expr)} as Lifted<number>))"
    case ClampExpression(target, lower, upper) =>
      s"${RUNTIME}.clamp(${compile(pb, target)}, ${compile(pb, lower)}, ${compile(pb, upper)})"
    case MathOpExpression(op, args) =>
      import MathOpExpressionOperator.*
      // Operands are spec-typed numbers — cast (as with AO args / substring) so
      // they still type after equality lost its narrowing.
      def n(e: Expression): String = s"(${compile(pb, e)} as Lifted<number>)"
      (op, args) match
        case (Neg, List(e))    => s"${RUNTIME}.negate(${n(e)})"
        case (Add, List(l, r)) => s"${RUNTIME}.add(${n(l)}, ${n(r)})"
        case (Mul, List(l, r)) => s"${RUNTIME}.multiply(${n(l)}, ${n(r)})"
        case (Sub, List(l, r)) => s"${RUNTIME}.subtract(${n(l)}, ${n(r)})"
        case (Pow, List(l, r)) =>
          s"${RUNTIME}.exponentiate(${n(l)}, ${n(r)})"
        case _ => ???
    case BitwiseExpression(l, op, r) =>
      s"${RUNTIME}.${compile(op)}(${compile(pb, l)}, ${compile(pb, r)})"
    case AbstractClosureExpression(params, captured, body) =>
      val funcBody =
        s"${RUNTIME}.default<Unlifted<Function>>( /* ABSTRACT_CLOSURE */ (${params
          .map(compile)
          .mkString(", ")}) => ${compileWithScope(pb, body)} , [${captured.map(compile(pb, _)).mkString(", ")}])"
      s"(() => {var _self = $funcBody; return _self as Lifted<Function>;})() as Lifted<Function>" // return IIFE
    case XRefExpression(
          XRefExpressionOperator.Algo | XRefExpressionOperator.Definition |
          XRefExpressionOperator.InternalMethod,
          id,
        ) =>
      println(spec.getAlgoById(id).head.fname)
      val fname = spec
        .getAlgoById(id)
        .head
        .fname
        .stripPrefix("INTRINSICS.yet:")
        .replace("`", "")
        .replace(".", "")
      s"${AO_HEADER}__${fname}"
    case XRefExpression(XRefExpressionOperator.ParamLength, id) =>
      spec.getAlgoById(id).head.originalParams.length.toString
    case XRefExpression(kind, id)    => ???
    case SoleElementExpression(list) => ???
    case CodeUnitAtExpression(base, index) =>
      s"${RUNTIME}.codeUnitAt(${compile(pb, base)}, ${compile(pb, index)})"
    case lit: Literal            => compile(lit)
    case MetaExpression(name, _) => ???
  }

  /** compile iterable of expressions */
  def compile(
    pb: PolyfillBuilder,
    iterable: Iterable[Expression],
    sep: String = ", ",
  ): String =
    iterable.map(compile(pb, _)).mkString(sep)

  /** compile binary operators */
  // operators now resolve to SpecRuntime method names (called as `$.<name>(l, r)`)
  def compile(op: BinaryExpressionOperator): String =
    import BinaryExpressionOperator.*
    op match {
      case Add => "add"
      case Sub => "subtract"
      case Mul => "multiply"
      case Div => "divide"
      case Mod => "remainder"
    }

  /** compile unary operators */
  def compile(op: UnaryExpressionOperator): String = op match
    case UnaryExpressionOperator.Neg => "negate"

  /** compile bitwise operations */
  def compile(op: BitwiseExpressionOperator): String = op match
    case BitwiseExpressionOperator.BAnd => "bitwiseAND"
    case BitwiseExpressionOperator.BOr  => "bitwiseOR"
    case BitwiseExpressionOperator.BXOr => "bitwiseXOR"

  /** compile mathematical function operators */
  def compile(op: MathFuncExpressionOperator): String =
    import MathFuncExpressionOperator.*
    op match {
      case Max      => "max"
      case Min      => "min"
      case Abs      => "abs"
      case Floor    => "floor"
      case Truncate => "truncate"
    }

  /** wrap an ordering-comparison expression (a Lifted<boolean>) so it is
    * recorded as a flippable path constraint and unlifted to a raw boolean at
    * its branch site. Mirrors `D$.C(id, op, value)`.
    */
  private def branchWithUnlift(pb: PolyfillBuilder, cmp: String): String =
    s"${RUNTIME}.value(${RUNTIME}.condition(Number.MAX_SAFE_INTEGER - ${newBranchId}, $cmp))"

  /** get next branch id */
  private def newBranchId: Int = {
    val bid = branchCount; branchCount += 1; bid
  }

  // branch id counter
  private var branchCount: Int = 0

  /** compile branch conditions */
  def compile(pb: PolyfillBuilder, cond: Condition): String = cond match {
    case ExpressionCondition(expr) =>
      // A bare expression in condition position evaluates to a Lifted<boolean>
      // (e.g. a manual `polyfill-rule.json` override like `$.is(...)`, or a
      // boolean reference). Native `if`/`&&` would see the Lifted object as
      // always-truthy, so funnel it through `$.condition(bid, ...)` to recover a
      // raw boolean (and record a flippable path constraint) — exactly like the
      // structured comparisons below. `$.condition` unwraps raw booleans too, so
      // overrides that already reduce to a native boolean stay correct.
      branchWithUnlift(pb, compile(pb, expr))
    case TypeCheckCondition(expr, neg, tys) =>
      val compiledExpr = compile(pb, expr)
      // Every spec type-check routes through the runtime predicate `$.isType`,
      // which owns each type's membership (e.g. "object" excludes null / includes
      // callables — a bare `typeof` is wrong there).
      (if (neg) s"!" else "") + tys
        .map(_.normalizedName.toLowerCase())
        .map(tyStr =>
          tyStr match {
            case "record[object]" => "object"
            case "record[symbol]" => "symbol"
            case _                => tyStr
          },
        )
        .map {
          // "an integral Number" (NumberInt) is not a `typeof`-checkable runtime
          // kind — it is truncate(ℝ(x)) == ℝ(x). The runtime owns it via the
          // `$.isInteger` predicate. Every predicate now returns a Lifted<boolean>,
          // so funnel each through `$.condition` (like the ordering comparisons) to
          // record a flippable constraint and unwrap to a raw boolean at the branch.
          case "numberint" =>
            branchWithUnlift(pb, s"${RUNTIME}.isInteger($compiledExpr)")
          case tyStr =>
            branchWithUnlift(
              pb,
              s"""${RUNTIME}.isType($compiledExpr, "$tyStr")""",
            )
        }
        .mkString("(", "||", ")")
    case HasFieldCondition(ref, neg, field, form, opTy) =>
      // Boxed-primitive internal slots ([[StringData]]/[[NumberData]]/…) aren't
      // in the object model, so `"Slot" in obj` is meaningless. Approximate on
      // the raw value: a boxed primitive is `instanceof` its constructor.
      val ctor = (form, field) match
        case (HasFieldConditionForm.InternalSlot, List(FieldLiteral(slot))) =>
          boxedSlotCtor.get(slot)
        case _ => None
      ctor match
        case Some(c) =>
          (if (neg) "!" else "") + s"(${RUNTIME}.value(${compile(pb, ref)}) instanceof $c)"
        case None =>
          (if (neg) s"!" else "") + s"(${compile(pb, field)} in ${compile(pb, ref)})"
    case HasBindingCondition(ref, neg, binding)    => ???
    case ProductionCondition(nt, lhsName, rhsName) => ???
    case PredicateCondition(expr, neg, op) =>
      import PredicateConditionOperator.*
      op match {
        case Finite =>
          (if (neg) s"!" else "") + branchWithUnlift(
            pb,
            s"${RUNTIME}.isFinite(${compile(pb, expr)})",
          )
        case Abrupt      => ???
        case Throw       => ???
        case Return      => ???
        case Break       => ???
        case Continue    => ???
        case NeverAbrupt => ???
        case Normal      => ???
        case Duplicated  => ???
        case Present => (if (neg) s"!" else "") + compile(pb, expr) + IS_PRESENT
        // A List "is empty" iff it has no elements (Lists compile to JS arrays).
        case Empty =>
          (if (neg) s"!" else "") + s"(${compile(pb, expr)}.length === 0)"
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
              case NumberLiteral(n) if n.isNaN =>
                branchWithUnlift(pb, s"${RUNTIME}.isNaN($l as Lifted<number>)")
              case _ =>
                branchWithUnlift(
                  pb,
                  s"${RUNTIME}.is($l, ${compile(pb, rexpr)})",
                ),
          )
          .reduce((l, r) => s"($l || $r)")
        (if (neg) s"!" else "") + e
      }
      es.reduce((l, r) => s"($l && $r)")
    case BinaryCondition(left, op, right) =>
      import BinaryConditionOperator.*
      lazy val l = compile(pb, left)
      lazy val r = compile(pb, right)
      // Every comparison returns a Lifted<boolean> (carrying its Sym); funnel
      // each through `$.condition(bid, ...)` at the branch site so it becomes a
      // flippable path constraint AND unwraps to a raw boolean for native control
      // flow. Equality included (`$.is`/`$.isNot` no longer narrow), so a string
      // `candidate === search` inside a search loop is now a real constraint.
      op match {
        case Eq       => branchWithUnlift(pb, s"${RUNTIME}.is($l, $r)")
        case NEq      => branchWithUnlift(pb, s"${RUNTIME}.isNot($l, $r)")
        case LessThan => branchWithUnlift(pb, s"${RUNTIME}.lessThan($l, $r)")
        case LessThanEqual =>
          branchWithUnlift(pb, s"${RUNTIME}.lessThanEqual($l, $r)")
        case GreaterThan =>
          branchWithUnlift(pb, s"${RUNTIME}.greaterThan($l, $r)")
        case GreaterThanEqual =>
          branchWithUnlift(pb, s"${RUNTIME}.greaterThanEqual($l, $r)")
        case SameCodeUnits => branchWithUnlift(pb, s"${RUNTIME}.is($l, $r)")
      }
    case InclusiveIntervalCondition(left, neg, from, to, _) =>
      val l = compile(pb, left)
      // Each bound is its own ordering comparison -> wrap each in `$.condition`
      // (raw boolean) so the native `&&` short-circuits correctly and both
      // bounds are independently flippable.
      val lo =
        branchWithUnlift(
          pb,
          s"${RUNTIME}.greaterThanEqual($l, ${compile(pb, from)})",
        )
      val hi = branchWithUnlift(
        pb,
        s"${RUNTIME}.lessThanEqual($l, ${compile(pb, to)})",
      )
      val e = s"($lo && $hi)"
      (if (neg) s"!" else "") + e
    case ContainsCondition(list, neg, ContainsConditionTarget.Expr(target)) =>
      // `contains` is a condition like any other comparison: funnel it through
      // `$.condition(bid, ...)` so the branch is flippable and the analysis
      // records the path constraint. The op returns a Lifted<boolean> (List
      // membership or String substring, dispatched at runtime).
      val c =
        s"${RUNTIME}.contains(${compile(pb, list)}, ${compile(pb, target)})"
      (if (neg) s"!" else "") + branchWithUnlift(pb, c)
    case ContainsCondition(list, neg, _) => ???
    case CompoundCondition(left, op, right) =>
      import CompoundConditionOperator.*
      lazy val l = compile(pb, left)
      lazy val r = compile(pb, right)
      op match
        case And   => s"$l && $r"
        case Or    => s"$l || $r"
        case Imply => ???
    case MetaCondition(name, _) => ???
  }

  /** escape a raw string for embedding in a TS double-quoted literal —
    * code-unit literals can be `"`, `\`, newline, etc. (e.g. 0x0022, 0x000A
    * from JSON serialization), which would otherwise break the emitted string.
    */
  private def tsStringLit(s: String): String =
    val sb = new StringBuilder("\"")
    s.foreach {
      case '\\'         => sb ++= "\\\\"
      case '"'          => sb ++= "\\\""
      case '\n'         => sb ++= "\\n"
      case '\r'         => sb ++= "\\r"
      case '\t'         => sb ++= "\\t"
      case c if c < ' ' => sb ++= f"\\u${c.toInt}%04x"
      case c            => sb += c
    }
    sb += '"'
    sb.toString

  def compile(lit: Literal): String =
    type ES_TYPE = "string" | "number" | "bigint" | "boolean" | "undefined" |
      "null" | "symbol"
    def w(s: String, ty: ES_TYPE): String = s"${RUNTIME}.default<$ty>($s, [])"
    lit match {
      case _: ThisLiteral          => THIS_PARAM
      case _: ThisParseNodeLiteral => ???
      case _: NewTargetLiteral =>
        "$.default<Unlifted<unknown>>(new.target as unknown as Unlifted<unknown>, [])"
      case HexLiteral(hex, _, _, _) =>
        w(tsStringLit(hex.toChar.toString), "string")
      case CodeLiteral(code)                 => w(tsStringLit(code), "string")
      case GrammarSymbolLiteral(name, flags) => ???
      case NonterminalLiteral(ordinal, name, flags, hasArticle) => ???
      case EnumLiteral(name) =>
        name match
          case "not-found" => w("-1", "number")
          case _           => w(tsStringLit(name), "string")
      case StringLiteral(str, _) => w(tsStringLit(str), "string")
      case FieldLiteral(name) =>
        s"\"$name\" /* TODO INTERNAL slots cannot be modeled */"
      case SymbolLiteral(sym)          => w(s"Symbol.$sym", "symbol")
      case ProductionLiteral(lhs, rhs) => ???
      case ErrorObjectLiteral(name) =>
        name match {
          case "AggregateError" => s"new $name(errors)"
          case _                => s"new $name()"
        }
      case _: PositiveInfinityMathValueLiteral => w("Infinity", "number")
      case _: NegativeInfinityMathValueLiteral => w("-Infinity", "number")
      case DecimalMathValueLiteral(n)          => w(s"$n", "number")
      case MathConstantLiteral(pre, name)      => ???
      case NumberLiteral(n) =>
        w(if (n.toInt == n) s"${n.toInt}" else s"$n", "number")
      case BigIntLiteral(n)        => w(s"${n}n", "bigint")
      case _: TrueLiteral          => w("true", "boolean")
      case _: FalseLiteral         => w("false", "boolean")
      case _: UndefinedLiteral     => w("undefined", "undefined")
      case _: NullLiteral          => w("null", "null")
      case _: UndefinedTypeLiteral => ???
      case _: NullTypeLiteral      => ???
      case _: BooleanTypeLiteral   => ???
      case _: StringTypeLiteral    => ???
      case _: SymbolTypeLiteral    => ???
      case _: NumberTypeLiteral    => ???
      case _: BigIntTypeLiteral    => ???
      case _: ObjectTypeLiteral    => ???
    }
}
