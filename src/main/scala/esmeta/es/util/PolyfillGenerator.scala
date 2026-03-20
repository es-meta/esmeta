package esmeta.es.util

import esmeta.es.*
import esmeta.lang.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker, Walker => LangWalker}
import esmeta.spec.*
import esmeta.util.BaseUtils.*

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

trait OptimizationPath {
  def apply(targets: List[Algorithm]): List[Algorithm]
}

class ShorthandInlinePath(spec: Spec) extends OptimizationPath {
  override def apply(targets: List[Algorithm]): List[Algorithm] = {
    targets.map { algo =>
      val inlinedBody = new LangWalker {
        override def walk(step: Step): Step = step match
          case InvokeShorthandStep(name, args) =>
            val shorthandAlgo = spec.fnameMap(name)
            val targetParameters = shorthandAlgo.head.originalParams.map(_.name)
            (targetParameters zip args).foldLeft(shorthandAlgo.body) {
              case (acc, (param, arg)) =>
                ParameterInlineWalker(param, arg).walk(acc)
            }
          case _ => super.walk(step)
      }.walk(algo.body)
      algo.copy(body = inlinedBody)
    }
  }

  private class ParameterInlineWalker(
    paramName: String,
    replaceWith: Expression,
  ) extends LangWalker {
    override def walk(expr: Expression): Expression = expr match {
      case ReferenceExpression(ref) =>
        ref match {
          case Variable(name, None) =>
            if (name == paramName) replaceWith else expr
          case x => ReferenceExpression(walk(x))
        }
      case _ => super.walk(expr)
    }

    override def walk(ref: Reference): Reference = ref match {
      case Variable(name, _) =>
        if (name == paramName) {
          replaceWith.asInstanceOf[ReferenceExpression].ref
        } else ref
      case x => super.walk(x)
    }
  }
}

class CompletionPath extends OptimizationPath {
  override def apply(targets: List[Algorithm]): List[Algorithm] = {
    val inspector = new PolyfillInspector(targets)

    targets.map { algo =>
      val newHead = inspector.transformHead(algo.head)
      val transformedBody = inspector.transformBody(algo.head, algo.body)
      algo.copy(head = newHead, body = transformedBody)
    }
  }
}

/** extensible helper of polyfill generator */
class PolyfillGenerator(spec: Spec, dslDir: Option[String]) {

  import Polyfill.*, PolyfillGenerator.*

  /** generated polyfills */
  lazy val result: List[Polyfill] =
    val optimizedTargets = optPaths.foldLeft(targets) { (x, optim) => optim(x) }
    for { algo <- optimizedTargets } yield compile(algo)

  /** list of optimization paths */
  val optPaths: List[OptimizationPath] = List(
    ShorthandInlinePath(spec),
    CompletionPath(),
  ) ++ (dslDir.map(dsl.DSLPath(_)))

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
            result += spec.getAlgoById(id)
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
  private val INTERNAL_HEADER = "IN";
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
    Polyfill(name, params, prelude ++ body)

  def compilePrelude(pb: PolyfillBuilder, head: Head, body: Step): Stmt =
    pb.newScope({
      val existenceCheckVariables = {
        var result = mutable.Set[String]()
        new LangUnitWalker {
          override def walk(cond: Condition): Unit =
            import PredicateConditionOperator.*
            cond match
              case PredicateCondition(
                    ReferenceExpression(Variable(name, _)),
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
    case MetaStep(name, multiline) => ???
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
    case RunningExecutionContext() => "this" // TODO Single-Runtime Assumption
    case SecondExecutionContext()  => ???
    case CurrentRealmRecord()      => "globalThis"
    case ActiveFunctionObject()    => "_self"
    case AgentRecord()             => ???
    case MetaReference(name)       => ???
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
    case StringExpression(expr) => compile(pb, expr)
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
      s"throw new Error(\"YET: ${str.replace("\"", "\\\"")}\")"
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
    case MathOpExpression(op, args) =>
      import MathOpExpressionOperator.*
      (op, args) match
        case (Neg, List(e))    => s"-${compile(pb, e)}"
        case (Add, List(l, r)) => s"${compile(pb, l)} + ${compile(pb, r)}"
        case (Mul, List(l, r)) => s"${compile(pb, l)} * ${compile(pb, r)}"
        case (Sub, List(l, r)) => s"${compile(pb, l)} - ${compile(pb, r)}"
        case (Pow, List(l, r)) =>
          s"${INTERNAL_HEADER}__pow(${compile(pb, l)}, ${compile(pb, r)})"
        case _ => ???
    case BitwiseExpression(l, op, r) =>
      s"${compile(pb, l)} ${compile(op)} ${compile(pb, r)}"
    case AbstractClosureExpression(params, captured, body) =>
      s"function _self(${params.map(compile).mkString(", ")}) ${compileWithScope(pb, body)}"
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
      s"${compile(pb, base)}[\"${compile(pb, index)}\"]"
    case lit: Literal         => compile(lit)
    case MetaExpression(name) => ???
  }

  /** compile iterable of expressions */
  def compile(
    pb: PolyfillBuilder,
    iterable: Iterable[Expression],
    sep: String = ", ",
  ): String =
    iterable.map(compile(pb, _)).mkString(sep)

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
  def compile(op: BitwiseExpressionOperator): String = op match
    case BitwiseExpressionOperator.BAnd => "&"
    case BitwiseExpressionOperator.BOr  => "|"
    case BitwiseExpressionOperator.BXOr => "^"

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
        .map(tyStr =>
          if (tyStr == "object") s"AO__IsObject($compiledExpr)"
          else s"typeof $compiledExpr === \"$tyStr\"",
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
          (if (neg) s"!" else "") + s"isFinite(${compile(pb, expr)})"
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
    case MetaCondition(name) => ???
  }

  def compile(lit: Literal): String =
    lit match {
      case _: ThisLiteral                    => "this"
      case _: ThisParseNodeLiteral           => ???
      case _: NewTargetLiteral               => "new.target"
      case HexLiteral(hex, _, _, _)          => s"\"${hex.toChar.toString}\""
      case CodeLiteral(code)                 => s"\"$code\""
      case GrammarSymbolLiteral(name, flags) => ???
      case NonterminalLiteral(ordinal, name, flags, hasArticle) => ???
      case EnumLiteral(name)                                    => s"\"$name\""
      case StringLiteral(str, _)                                => s"\"$str\""
      case FieldLiteral(name)                                   => s"\"$name\""
      case SymbolLiteral(sym)          => s"Symbol.$sym"
      case ProductionLiteral(lhs, rhs) => ???
      case ErrorObjectLiteral(name) =>
        name match {
          case "AggregateError" => s"new $name(errors)"
          case _                => s"new $name()"
        }
      case _: PositiveInfinityMathValueLiteral => "Infinity"
      case _: NegativeInfinityMathValueLiteral => "-Infinity"
      case DecimalMathValueLiteral(n)          => s"$n"
      case MathConstantLiteral(pre, name)      => ???
      case NumberLiteral(n)        => if (n.toInt == n) s"${n.toInt}" else s"$n"
      case BigIntLiteral(n)        => s"${n}n"
      case _: TrueLiteral          => "true"
      case _: FalseLiteral         => "false"
      case _: UndefinedLiteral     => "undefined"
      case _: NullLiteral          => "null"
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
