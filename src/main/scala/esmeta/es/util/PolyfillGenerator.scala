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
    """INTRINSICS\.(get:|set:)?String\..*""",
    // https://tc39.es/ecma262/#sec-properties-of-the-array-prototype-object
    """INTRINSICS\.(get:|set:)?Array\..*""",
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
  )

  val ignoreTargets = List(
    // ES3
    // "INTRINSICS.String.prototype.charAt",
    // "INTRINSICS.String.prototype.charCodeAt",
    // "INTRINSICS.String.prototype.concat",
    // "INTRINSICS.String.prototype.indexOf",
    // "INTRINSICS.String.prototype.lastIndexOf",
    // "INTRINSICS.String.prototype.localeCompare",
    // "INTRINSICS.String.prototype.match",
    // "INTRINSICS.String.prototype.replace",
    // "INTRINSICS.String.prototype.search",
    // "INTRINSICS.String.prototype.slice",
    // "INTRINSICS.String.prototype.split",
    // "INTRINSICS.String.prototype.substring",
    // "INTRINSICS.String.prototype.toLocaleLowerCase",
    // "INTRINSICS.String.prototype.toLocaleUpperCase",
    // "INTRINSICS.String.prototype.toLowerCase",
    // "INTRINSICS.String.prototype.toString",
    // "INTRINSICS.String.prototype.toUpperCase",
    // "INTRINSICS.String.prototype.valueOf",

    // Unsupported
    "INTRINSICS.MapIteratorPrototype.next",
    "INTRINSICS.SetIteratorPrototype.next",

    // Yet AOs
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
    * `instanceof`. These slots aren't represented in the object model, so a spec
    * "_x_ has a [[…Data]] internal slot" check is approximated on the raw value. */
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
    Polyfill(
      name,
      params,
      prelude ++ body,
      hasThis = hasThis,
      isAbstractOp = isAbstractOp,
      aoImports = aoImports,
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
          s"${INTERNAL_HEADER}__Prepend(${compile(pb, ref)}, ${compile(pb, expr)})",
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
    case x: Variable                => compile(x)
    case Access(base, name, _, _)   => s"${compile(pb, base)}[\"$name\"]"
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
        s"(${compile(pb, t)} as Wrapped<number>)",
      )
      s"${RUNTIME}.substring($base, (${compile(pb, from)} as Wrapped<number>), $end)"
    case TrimExpression(expr, leading, trailing) =>
      s"${RUNTIME}.trim(${compile(pb, expr)}, $leading, $trailing)"
    case NumberOfExpression(_, _, ReferenceExpression(ref), _) =>
      // a List's length is a value too — wrap it so it flows through the ops.
      s"${RUNTIME}.base<number>(${compile(pb, ref)}.length, [])"
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
    case expr: GetItemsExpression                           => ???
    case InvokeAbstractOperationExpression(name, args, tag) =>
      // Cast each argument to the callee's declared parameter type. AO calls are
      // spec-typed contracts, so this is a "trust the frontend" cast that closes
      // TS control-flow-narrowing gaps (e.g. a value the spec proves is a String
      // but TS still sees as Wrapped<unknown> across correlated conditions).
      val params =
        spec.fnameMap.get(name).map(_.head.originalParams).getOrElse(Nil)
      val argStrs = args.zipWithIndex.map { (arg, i) =>
        val c = compile(pb, arg)
        params.lift(i).fold(c)(p => s"($c as ${Polyfill.tsParamType(p.ty)})")
      }
      s"${AO_HEADER}__$name(${(RUNTIME :: argStrs).mkString(", ")})"
    case InvokeNumericMethodExpression(ty, name, args) =>
      s"${ty}__$name(${compile(pb, args)})"
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
        // `Wrapped<never>[]` (= never[]) is assignable to any list param, and
        // `$.append` still pins its element type from the pushed value.
        case EmptyList(isNewUsed, typeDesc) => "[] as Wrapped<never>[]"
        case IntRange(
              from,
              isFromInclusive,
              to,
              isToInclusive,
              isAscending,
            ) =>
          s"${INTERNAL_HEADER}__IntRange(${compile(pb, from)}, $isFromInclusive, ${compile(pb, to)}, $isToInclusive, $isAscending)"
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
      s"${RUNTIME}.${compile(op)}((${compile(pb, left)} as Wrapped<number>), (${compile(pb, right)} as Wrapped<number>))"
    case UnaryExpression(op, expr) =>
      s"${RUNTIME}.${compile(op)}((${compile(pb, expr)} as Wrapped<number>))"
    case ClampExpression(target, lower, upper) =>
      s"${RUNTIME}.clamp(${compile(pb, target)}, ${compile(pb, lower)}, ${compile(pb, upper)})"
    case MathOpExpression(op, args) =>
      import MathOpExpressionOperator.*
      // Operands are spec-typed numbers — cast (as with AO args / substring) so
      // they still type after equality lost its narrowing.
      def n(e: Expression): String = s"(${compile(pb, e)} as Wrapped<number>)"
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
        s"(${params.map(compile).mkString(", ")}) => ${compileWithScope(pb, body)}"
      s"(() => {var _self = $funcBody; return _self;})()" // return IIFE
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

  /** wrap an ordering-comparison expression (a Wrapped<boolean>) so it is
    * recorded as a flippable path constraint and unwrapped to a raw boolean at
    * its branch site. Mirrors `D$.C(id, op, value)`.
    */
  private def branch(pb: PolyfillBuilder, cmp: String): String =
    s"${RUNTIME}.condition(Number.MAX_SAFE_INTEGER - ${newBranchId}, $cmp)"

  /** get next branch id */
  private def newBranchId: Int = {
    val bid = branchCount; branchCount += 1; bid
  }

  // branch id counter
  private var branchCount: Int = 0

  /** compile branch conditions */
  def compile(pb: PolyfillBuilder, cond: Condition): String = cond match {
    case ExpressionCondition(expr) => compile(pb, expr)
    case TypeCheckCondition(expr, neg, tys) =>
      val compiledExpr = compile(pb, expr)
      // Every spec type-check routes through the runtime predicate `$.isType`,
      // which owns each type's membership (e.g. "object" excludes null / includes
      // callables — a bare `typeof` is wrong there).
      (if (neg) s"!" else "") + tys
        .map(_.normalizedName.toLowerCase())
        .map(tyStr => if (tyStr == "record[object]") "object" else tyStr)
        .map(tyStr => s"""${RUNTIME}.isType($compiledExpr, "$tyStr")""")
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
          (if (neg) "!" else "") + s"(${RUNTIME}.peek(${compile(pb, ref)}) instanceof $c)"
        case None =>
          (if (neg) s"!" else "") + s"(${compile(pb, field)} in ${compile(pb, ref)})"
    case HasBindingCondition(ref, neg, binding)    => ???
    case ProductionCondition(nt, lhsName, rhsName) => ???
    case PredicateCondition(expr, neg, op) =>
      import PredicateConditionOperator.*
      op match {
        case Finite =>
          (if (neg) s"!" else "") + s"${RUNTIME}.isFinite(${compile(pb, expr)})"
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
                s"${RUNTIME}.isNaN($l as Wrapped<number>)"
              case _ => branch(pb, s"${RUNTIME}.is($l, ${compile(pb, rexpr)})"),
          )
          .reduce((l, r) => s"($l || $r)")
        (if (neg) s"!" else "") + e
      }
      es.reduce((l, r) => s"($l && $r)")
    case BinaryCondition(left, op, right) =>
      import BinaryConditionOperator.*
      lazy val l = compile(pb, left)
      lazy val r = compile(pb, right)
      // Every comparison returns a Wrapped<boolean> (carrying its Sym); funnel
      // each through `$.condition(bid, ...)` at the branch site so it becomes a
      // flippable path constraint AND unwraps to a raw boolean for native control
      // flow. Equality included (`$.is`/`$.isNot` no longer narrow), so a string
      // `candidate === search` inside a search loop is now a real constraint.
      op match {
        case Eq            => branch(pb, s"${RUNTIME}.is($l, $r)")
        case NEq           => branch(pb, s"${RUNTIME}.isNot($l, $r)")
        case LessThan      => branch(pb, s"${RUNTIME}.lessThan($l, $r)")
        case LessThanEqual => branch(pb, s"${RUNTIME}.lessThanEqual($l, $r)")
        case GreaterThan   => branch(pb, s"${RUNTIME}.greaterThan($l, $r)")
        case GreaterThanEqual =>
          branch(pb, s"${RUNTIME}.greaterThanEqual($l, $r)")
        case SameCodeUnits => branch(pb, s"${RUNTIME}.is($l, $r)")
      }
    case InclusiveIntervalCondition(left, neg, from, to, _) =>
      val l = compile(pb, left)
      // Each bound is its own ordering comparison -> wrap each in `$.condition`
      // (raw boolean) so the native `&&` short-circuits correctly and both
      // bounds are independently flippable.
      val lo =
        branch(pb, s"${RUNTIME}.greaterThanEqual($l, ${compile(pb, from)})")
      val hi = branch(pb, s"${RUNTIME}.lessThanEqual($l, ${compile(pb, to)})")
      val e = s"($lo && $hi)"
      (if (neg) s"!" else "") + e
    case ContainsCondition(list, neg, ContainsConditionTarget.Expr(target)) =>
      val c =
        s"${RUNTIME}.contains(${compile(pb, list)}, ${compile(pb, target)})"
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
    case MetaCondition(name, _) => ???
  }

  /** escape a raw string for embedding in a TS double-quoted literal — code-unit
    * literals can be `"`, `\`, newline, etc. (e.g. 0x0022, 0x000A from JSON
    * serialization), which would otherwise break the emitted string. */
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
    // Literals denoting ECMAScript values are wrapped (`$.base(v, [])`) so they
    // flow through the value ops like any other Wrapped value; this also keeps a
    // variable's type consistent across branches (e.g. `var ref` assigned a
    // string literal in one branch and a Wrapped<string> in another). Structural
    // literals (this/new.target/field keys/error constructors) pass through raw.
    // Widen the payload type (`base<string>` not `base<"$$">`) so a variable
    // assigned different literals across branches keeps a single Wrapped<string>.
    def w(s: String, ty: String): String = s"${RUNTIME}.base<$ty>($s, [])"
    lit match {
      case _: ThisLiteral          => THIS_PARAM
      case _: ThisParseNodeLiteral => ???
      case _: NewTargetLiteral     => "new.target"
      case HexLiteral(hex, _, _, _) =>
        w(tsStringLit(hex.toChar.toString), "string")
      case CodeLiteral(code)                 => w(tsStringLit(code), "string")
      case GrammarSymbolLiteral(name, flags) => ???
      case NonterminalLiteral(ordinal, name, flags, hasArticle) => ???
      case EnumLiteral(name)     => w(tsStringLit(name), "string")
      case StringLiteral(str, _) => w(tsStringLit(str), "string")
      case FieldLiteral(name) =>
        s"\"$name\" /* TODO internal slots cannot be modeled */"
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
