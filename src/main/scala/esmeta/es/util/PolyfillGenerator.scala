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

      head.originalParams.zipWithIndex.foreach((param, index) => {
        if (existenceCheckVariables.contains(param.name))
          pb.addStmt(
            NormalStmt(
              s"var ${param.name}$IS_PRESENT = arguments.length > $index;",
            ),
          )
      })

      head.originalParams.zipWithIndex
        .foreach((param, index) => {
          if (param.kind == ParamKind.Optional)
            pb.addStmt(
              NormalStmt(
                s"var ${param.name} = arguments.length > $index ? arguments[$index] : undefined;",
              ),
            )
        })
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
    case RunningExecutionContext() => "this" // ??? TODO Single-Runtime Assumption
    case SecondExecutionContext()  => ???
    case CurrentRealmRecord()      => "globalThis"
    case ActiveFunctionObject()    => "_self"
    case AgentRecord()             => ???
    case MetaReference(name, _)    => ???
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
    case LengthExpression(expr) => ???
    case StringExpression(expr) => compile(pb, expr)
    case SubstringExpression(expr, from, to) =>
      // An omitted `to` means "to the end of the string"; emit its length so the
      // 3-arg runtime `substring(s, from, to)` always gets a concrete end index.
      val base = compile(pb, expr)
      val end = to.fold(s"${RUNTIME}.length($base)")(compile(pb, _))
      s"${RUNTIME}.substring($base, ${compile(pb, from)}, $end)"
    case TrimExpression(expr, leading, trailing) =>
      s"${INTERNAL_HEADER}__Trim(${compile(pb, expr)}, $leading, $trailing)"
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
    case expr: GetItemsExpression => ???
    case InvokeAbstractOperationExpression(name, args, tag) =>
      // Cast each argument to the callee's declared parameter type. AO calls are
      // spec-typed contracts, so this is a "trust the frontend" cast that closes
      // TS control-flow-narrowing gaps (e.g. a value the spec proves is a String
      // but TS still sees as Wrapped<unknown> across correlated conditions).
      val params = spec.fnameMap.get(name).map(_.head.originalParams).getOrElse(Nil)
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
        case LiteralSyntax(entries)         => s"[${compile(pb, entries)}]"
        case SoleElement(entry)             => s"[${compile(pb, entry)}]"
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
    case ReferenceExpression(ref)     => compile(pb, ref)
    case MathFuncExpression(op, args) => s"${RUNTIME}.${compile(op)}(${compile(pb, args)})"
    case ConversionExpression(op, expr, form) => compile(pb, expr)
    case ExponentiationExpression(base, power) =>
      s"${RUNTIME}.exponentiate(${compile(pb, base)}, ${compile(pb, power)})"
    case BinaryExpression(left, op, right) =>
      s"${RUNTIME}.${compile(op)}(${compile(pb, left)}, ${compile(pb, right)})"
    case UnaryExpression(op, expr) => s"${RUNTIME}.${compile(op)}(${compile(pb, expr)})"
    case ClampExpression(target, lower, upper) =>
      s"${RUNTIME}.clamp(${compile(pb, target)}, ${compile(pb, lower)}, ${compile(pb, upper)})"
    case MathOpExpression(op, args) =>
      import MathOpExpressionOperator.*
      (op, args) match
        case (Neg, List(e))    => s"${RUNTIME}.negate(${compile(pb, e)})"
        case (Add, List(l, r)) => s"${RUNTIME}.add(${compile(pb, l)}, ${compile(pb, r)})"
        case (Mul, List(l, r)) => s"${RUNTIME}.multiply(${compile(pb, l)}, ${compile(pb, r)})"
        case (Sub, List(l, r)) => s"${RUNTIME}.subtract(${compile(pb, l)}, ${compile(pb, r)})"
        case (Pow, List(l, r)) =>
          s"${RUNTIME}.exponentiate(${compile(pb, l)}, ${compile(pb, r)})"
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
  // operators now resolve to BootStrap method names (called as `$.<name>(l, r)`)
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

  /** compile branch conditions */
  def compile(pb: PolyfillBuilder, cond: Condition): String = cond match {
    case ExpressionCondition(expr) => compile(pb, expr)
    case TypeCheckCondition(expr, neg, tys) =>
      val compiledExpr = compile(pb, expr)
      (if (neg) s"!" else "") + tys
        .map(_.normalizedName.toLowerCase())
        .map(tyStr => if (tyStr == "record[object]") "object" else tyStr)
        .map(tyStr =>
          if (tyStr == "object") s"AO__IsObject($RUNTIME, $compiledExpr)"
          else s"${RUNTIME}.typeOf($compiledExpr) === \"$tyStr\"",
        )
        .mkString("(", "||", ")")
    case HasFieldCondition(ref, neg, field, form, opTy) =>
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
              case NumberLiteral(n) if n.isNaN => s"${RUNTIME}.isNaN($l)"
              case _ => s"${RUNTIME}.is($l, ${compile(pb, rexpr)})",
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
        case Eq               => s"${RUNTIME}.is($l, $r)"
        case NEq              => s"${RUNTIME}.isNot($l, $r)"
        case LessThan         => s"${RUNTIME}.lessThan($l, $r)"
        case LessThanEqual    => s"${RUNTIME}.lessThanEqual($l, $r)"
        case GreaterThan      => s"${RUNTIME}.greaterThan($l, $r)"
        case GreaterThanEqual => s"${RUNTIME}.greaterThanEqual($l, $r)"
        case SameCodeUnits    => s"${RUNTIME}.is($l, $r)"
      }
    case InclusiveIntervalCondition(left, neg, from, to, _) =>
      val l = compile(pb, left)
      val e =
        s"(${RUNTIME}.greaterThanEqual($l, ${compile(pb, from)}) && ${RUNTIME}.lessThanEqual($l, ${compile(pb, to)}))"
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
      case _: ThisLiteral                    => THIS_PARAM
      case _: ThisParseNodeLiteral           => ???
      case _: NewTargetLiteral               => "new.target"
      case HexLiteral(hex, _, _, _)          => w(s"\"${hex.toChar.toString}\"", "string")
      case CodeLiteral(code)                 => w(s"\"$code\"", "string")
      case GrammarSymbolLiteral(name, flags) => ???
      case NonterminalLiteral(ordinal, name, flags, hasArticle) => ???
      case EnumLiteral(name)                                    => w(s"\"$name\"", "string")
      case StringLiteral(str, _)                                => w(s"\"$str\"", "string")
      case FieldLiteral(name)                                   => s"\"$name\""
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
      case NumberLiteral(n)        => w(if (n.toInt == n) s"${n.toInt}" else s"$n", "number")
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
