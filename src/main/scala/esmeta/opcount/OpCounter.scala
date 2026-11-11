package esmeta.opcount

import esmeta.lang.*
import esmeta.lang.util.UnitWalker
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** Count the Table I operations that `PolyfillGenerator` emits for an
  * algorithm (one count per emitted `$.<op>` call), and collect the abstract
  * operations it invokes.
  *
  * Two facts need more than one step, so they are collected in a pre-pass:
  *   - `the number of elements in _S_` over a String (ES2016-) is `length`
  *   - `Append _cu_ to _L_` where _L_ later becomes a String (`the String value
  *     whose code units are the elements in the List _L_`) is `concatenate`
  */
class OpCounter extends UnitWalker {
  val ops: MMap[String, Int] = MMap().withDefaultValue(0)
  val calls: MSet[String] = MSet()
  private val strVars: MSet[String] = MSet()
  private val codeUnitLists: MSet[String] = MSet()

  private def add(op: String, n: Int = 1): Unit = if (n > 0) ops(op) += n

  // constant expressions (e.g., `2<sup>53</sup> - 1`, `-1`) are not counted
  private def isConst(e: Expression): Boolean = e match
    case _: Literal                     => true
    case UnaryExpression(_, e)          => isConst(e)
    case BinaryExpression(l, _, r)      => isConst(l) && isConst(r)
    case ExponentiationExpression(b, p) => isConst(b) && isConst(p)
    case MathOpExpression(_, args)      => args.forall(isConst)
    case _                              => false

  def count(step: Step): Unit =
    Prepass.walk(step)
    walk(step)

  // abstract operations producing Strings
  private val strAOs = Set("ToString", "thisStringValue", "ThisStringValue")
  private def isStr(e: Expression): Boolean = e match
    case ReturnIfAbruptExpression(e, _) => isStr(e)
    case InvokeAbstractOperationExpression(name, _, _) => strAOs(name)
    case _: StringConcatExpression | _: SubstringExpression |
        _: TrimExpression | _: StringLiteral | _: CodeLiteral |
        _: StringExpression =>
      true
    case _ => false

  private object Prepass extends UnitWalker {
    override def walk(step: Step): Unit =
      step match
        case LetStep(x, e) if isStr(e)                     => strVars += x.name
        case SetStep(x: Variable, e) if isStr(e)           => strVars += x.name
        case _                                             =>
      super.walk(step)
    override def walk(expr: Expression): Unit =
      expr match
        case StringExpression(ReferenceExpression(x: Variable)) =>
          codeUnitLists += x.name
        case _ =>
      super.walk(expr)
  }

  override def walk(step: Step): Unit =
    step match
      case AppendStep(_, x: Variable) if codeUnitLists(x.name) =>
        add("concatenate")
      case AppendStep(_, _)       => add("append")
      case PrependStep(_, _)      => add("prepend")
      case _: ForEachIntegerStep  => add("range")
      case _                      =>
    step match
      case _: AssertStep => // not compiled
      case _             => super.walk(step)

  override def walk(expr: Expression): Unit =
    expr match
      case StringConcatExpression(es) => add("concatenate", es.length - 1)
      case LengthExpression(_)        => add("length")
      case SubstringExpression(_, _, to) =>
        add("substring"); if (to.isEmpty) add("length")
      case TrimExpression(_, _, _) => add("trim")
      case NumberOfExpression(_, _, ReferenceExpression(x: Variable), _)
          if strVars(x.name) =>
        add("length")
      case CodeUnitAtExpression(_, _) => add("codeUnitAt")
      case ListExpression(_: ListExpressionForm.IntRange) => add("range")
      case ClampExpression(_, _, _) => add("clamp")
      case e: MathOpExpression if isConst(e) =>
      case MathOpExpression(op, _) =>
        import MathOpExpressionOperator.*
        op match
          case Neg => add("negate")
          case Add => add("add")
          case Sub => add("subtract")
          case Mul => add("multiply")
          case Pow => add("exponentiate")
          case _   => add(s"math:$op")
      case BitwiseExpression(_, op, _) =>
        import BitwiseExpressionOperator.*
        add(op match
          case BAnd => "bitwiseAND"
          case BOr  => "bitwiseOR"
          case BXOr => "bitwiseXOR",
        )
      case _                                              =>
    super.walk(expr)

  override def walk(expr: CalcExpression): Unit =
    if (!isConst(expr)) expr match
      case MathFuncExpression(op, _) => add(op.toString.toLowerCase)
      case ExponentiationExpression(_, _) => add("exponentiate")
      case BinaryExpression(_, op, _) =>
        import BinaryExpressionOperator.*
        add(op match
          case Add => "add"
          case Sub => "subtract"
          case Mul => "multiply"
          case Div => "divide"
          case Mod => "remainder",
        )
      case UnaryExpression(_, _) => add("negate")
      case _ =>
    super.walk(expr)

  override def walk(invoke: InvokeExpression): Unit =
    invoke match
      case InvokeAbstractOperationExpression(name, _, _) =>
        calls += name
        name match
          case "Get" | "GetV" => add("get")
          case "Set"          => add("set")
          case "Call"         => add("call")
          case _              =>
      case InvokeNumericMethodExpression(t, name, _) => calls += s"$t::$name"
      case _                                         =>
    super.walk(invoke)

  override def walk(cond: Condition): Unit =
    import BinaryConditionOperator.*
    cond match
      // `Type(_x_) is String` (ES2021-) is a type check (`_x_ is a String`)
      case IsAreCondition(ls, _, rs) =>
        for (l <- ls; r <- rs) (l, r) match
          case (InvokeAbstractOperationExpression("Type", _, _), _) =>
            add("isType")
          case (_, NumberLiteral(n)) if n.isNaN => add("isNaN")
          case _                                => add("is")
      case BinaryCondition(_, op, _) =>
        add(op match
          case Eq | SameCodeUnits => "is"
          case NEq                => "isNot"
          case LessThan           => "lessThan"
          case LessThanEqual      => "lessThanEqual"
          case GreaterThan        => "greaterThan"
          case GreaterThanEqual   => "greaterThanEqual",
        )
      case InclusiveIntervalCondition(_, _, _, _, _) =>
        add("greaterThanEqual"); add("lessThanEqual")
      case TypeCheckCondition(_, _, tys) =>
        for (ty <- tys)
          add(if (ty.normalizedName == "NumberInt") "isInteger" else "isType")
      case PredicateCondition(_, _, PredicateConditionOperator.Finite) =>
        add("isFinite")
      case ContainsCondition(_, _, ContainsConditionTarget.Expr(_)) =>
        add("contains")
      case _ =>
    super.walk(cond)
}
