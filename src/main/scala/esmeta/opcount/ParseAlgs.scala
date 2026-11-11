package esmeta.opcount

import esmeta.lang.*
import esmeta.lang.util.{Parser, LegacyParser, Parsers, UnitWalker}
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.parse as parseJson

/** Parse raw `<emu-alg>` bodies of several ECMA-262 editions with the current
  * metalanguage parser and report how many steps fall back to `YetStep`.
  *
  * input : {edition: {algorithm name: raw emu-alg text}}
  * output: {edition: {algorithm name: {steps, yetSteps, yetExprs, yets, ast,
  *   ops, blocked, kinds, calls, defaults}}}
  *
  * `--legacy` uses `LegacyParser` instead of `Parser`.
  */
object ParseAlgs {
  class Counter extends UnitWalker {
    var steps = 0
    var yetSteps = 0
    var yetExprs = 0
    val yets = scala.collection.mutable.ListBuffer[String]()
    override def walk(subStep: SubStep): Unit =
      steps += 1
      super.walk(subStep)
    override def walk(step: Step): Unit = step match
      case YetStep(YetExpression(str, block)) =>
        yetSteps += 1
        yets += str
        block.foreach(walk)
      case _ => super.walk(step)
    override def walk(expr: Expression): Unit = expr match
      case YetExpression(str, block) =>
        yetExprs += 1
        yets += s"[expr] $str"
        block.foreach(walk)
      case _ => super.walk(expr)
  }

  /** every metalanguage node kind (with operator, if any) in an algorithm */
  class Kinds extends UnitWalker {
    val kinds = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    private def name(x: Any): String = x.getClass.getSimpleName.stripSuffix("$")
    private def add(k: String): Unit = kinds(k) += 1
    override def walk(step: Step): Unit =
      add(s"step:${name(step)}"); super.walk(step)
    override def walk(expr: Expression): Unit =
      expr match
        case MathOpExpression(op, _) => add(s"expr:MathOp:$op")
        case BitwiseExpression(_, op, _) => add(s"expr:Bitwise:$op")
        case ListExpression(f) => add(s"expr:List:${name(f)}")
        case _: CalcExpression => // counted by walk(CalcExpression)
        case _ => add(s"expr:${name(expr)}")
      super.walk(expr)
    override def walk(expr: CalcExpression): Unit =
      expr match
        case BinaryExpression(_, op, _) => add(s"calc:Binary:$op")
        case MathFuncExpression(op, _) => add(s"calc:MathFunc:$op")
        case UnaryExpression(op, _) => add(s"calc:Unary:$op")
        case _ => add(s"calc:${name(expr)}")
      super.walk(expr)
    override def walk(cond: Condition): Unit =
      cond match
        case BinaryCondition(_, op, _) => add(s"cond:Binary:$op")
        case PredicateCondition(_, _, op) => add(s"cond:Predicate:$op")
        case CompoundCondition(_, op, _) => add(s"cond:Compound:$op")
        case _ => add(s"cond:${name(cond)}")
      super.walk(cond)
  }

  /** constructs that `PolyfillGenerator` cannot compile (its `???` cases),
    * outside `Assert` steps (which it skips) */
  class Blocked extends UnitWalker {
    val blocked = scala.collection.mutable.ListBuffer[String]()
    private def b(s: String): Unit = blocked += s
    override def walk(step: Step): Unit = step match
      case _: AssertStep => ()
      case RemoveStep(t, _, _) =>
        t match
          case RemoveStep.Target.First(None) | RemoveStep.Target.Last(None) =>
          case _ => b("RemoveStep:" + t)
        super.walk(step)
      case _ => super.walk(step)
    override def walk(expr: Expression): Unit =
      expr match
        case LengthExpression(_: ReferenceExpression) =>
        case LengthExpression(_) => b("Length(non-ref)")
        case NumberOfExpression(_, _, _: ReferenceExpression, _) =>
        case _: NumberOfExpression => b("NumberOf(non-ref)")
        case MathOpExpression(op, args) =>
          import MathOpExpressionOperator.*
          if (!Set(Neg, Add, Mul, Sub, Pow)(op)) b(s"MathOp:$op")
        case _: SoleElementExpression => b("SoleElement")
        case _: MetaExpression => b("Meta")
        case InvokeAbstractOperationExpression("Type", _, _) => b("AO:Type")
        case _ =>
      super.walk(expr)
    override def walk(lit: Literal): Unit = lit match
      case _: UndefinedTypeLiteral | _: NullTypeLiteral | _: BooleanTypeLiteral |
          _: StringTypeLiteral | _: SymbolTypeLiteral | _: NumberTypeLiteral |
          _: BigIntTypeLiteral | _: ObjectTypeLiteral => b("TypeLiteral")
      case _: MathConstantLiteral => b("MathConstant")
      case _ =>
    override def walk(ref: Reference): Unit =
      ref match
        case PositionalElement(_, false) => b("PositionalElement(last)")
        case _ =>
      super.walk(ref)
    override def walk(cond: Condition): Unit =
      cond match
        case ContainsCondition(_, _, _: ContainsConditionTarget.Expr) =>
        case _: ContainsCondition => b("Contains(such-that)")
        case CompoundCondition(_, CompoundConditionOperator.Imply, _) => b("Imply")
        case PredicateCondition(_, _, op) =>
          import PredicateConditionOperator.*
          if (!Set(Finite, Present, Empty)(op)) b(s"Predicate:$op")
        case _ =>
      super.walk(cond)
  }

  def main(args: Array[String]): Unit =
    val (in, out) = (args(0), args(1))
    val parser: Parsers = if (args.contains("--legacy")) LegacyParser else Parser
    val input = parseJson(readFile(in)).flatMap(_.as[Map[String, Map[String, String]]]) match
      case Right(m) => m
      case Left(e)  => throw e
    val result = for ((ed, algs) <- input) yield ed -> (for ((name, code) <- algs) yield {
      val c = Counter()
      var repr = ""
      val oc = OpCounter()
      val kd = Kinds()
      val bl = Blocked()
      val dc = DefaultCounter()
      val error =
        try {
          val step = parser.parseBy(parser.step)(code)
          repr = step.toString
          c.walk(step); oc.count(step); kd.walk(step); bl.walk(step); dc.walk(step); None
        } catch { case e: Throwable => Some(e.getMessage.take(200)) }
      name -> Json.obj(
        "steps" -> c.steps.asJson,
        "yetSteps" -> c.yetSteps.asJson,
        "yetExprs" -> c.yetExprs.asJson,
        "yets" -> c.yets.toList.asJson,
        "error" -> error.asJson,
        "ast" -> repr.asJson,
        "ops" -> oc.ops.toMap.asJson,
        "blocked" -> bl.blocked.toList.asJson,
        "kinds" -> kd.kinds.toMap.asJson,
        "calls" -> oc.calls.toList.sorted.asJson,
        "defaults" -> dc.sites.toMap.asJson,
      )
    }).toMap.asJson
    dumpFile(result.asJson.spaces2, out)
    for ((ed, algs) <- input.toList.sortBy(_._1)) println(ed)
}
