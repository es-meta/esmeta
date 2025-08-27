package esmeta.es.util

import esmeta.es.*
import esmeta.lang.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*

/** polyfill generator */
object PolyfillGenerator {
  def apply(spec: Spec): List[Polyfill] = new PolyfillGenerator(spec).result

  val defaultTargets = List(
    "Array.prototype.fill",
  )
}

/** extensible helper of polyfill generator */
class PolyfillGenerator(spec: Spec) {

  import Polyfill.*, PolyfillGenerator.*

  /** generated polyfills */
  lazy val result: List[Polyfill] = for {
    algo <- spec.algorithms
    if (
      algo.isBuiltin &&
      algo.complete &&
      defaultTargets.exists(algo.name.contains)
    )
  } yield compile(algo)

  /** compile an algorithm into a polyfill */
  def compile(algo: Algorithm): Polyfill =
    // TODO remove this after implementing all steps
    println(algo)
    println("-" * 80)
    val pb = PolyfillBuilder(spec, algo)
    val stmt = compileWithScope(pb, algo.body)
    Polyfill("" /* TODO */, stmt)

  /** compile with a new scope and convert it into a statement */
  def compileWithScope(pb: PolyfillBuilder, step: Step): Stmt = try {
    pb.newScope(compile(pb, step))
  } catch {
    // TODO remove this catch after implementing all steps
    case e: Throwable =>
      println(pb.currentResult)
      println("-" * 80)
      throw e
  }

  /** compile algorithm steps */
  def compile(
    pb: PolyfillBuilder,
    step: Step,
  ): Unit = step match {
    case LetStep(x, expr) =>
      pb.addStmt(NormalStmt(s"var ${compile(pb, x)} = ${compile(pb, expr)};"))
    case SetStep(x, expr)                           => ???
    case SetAsStep(x, verb, id)                     => ???
    case SetEvaluationStateStep(base, func, args)   => ???
    case PerformStep(expr)                          => ???
    case InvokeShorthandStep(x, a)                  => ???
    case AppendStep(expr, ref)                      => ???
    case PrependStep(expr, ref)                     => ???
    case AddStep(expr, ref)                         => ???
    case RemoveStep(t, p, l)                        => ???
    case PushContextStep(ref)                       => ???
    case SuspendStep(ref, rm)                       => ???
    case RemoveContextStep(ctxt, t)                 => ???
    case AssertStep(cond)                           => ???
    case IfStep(cond, thenStep, elseStep, config)   => ???
    case RepeatStep(cond, body)                     => ???
    case ForEachStep(ty, elem, expr, forward, body) => ???
    case ForEachIntegerStep(x, low, lowInc, high, highInc, ascending, body) =>
      ???
    case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
      ???
    case ForEachParseNodeStep(x, expr, body) => ???
    case ReturnStep(expr)                    => ???
    case ThrowStep(name) =>
      pb.addStmt(NormalStmt(s"throw new $name;"))
    case ResumeStep(callerCtxt, arg, genCtxt, param, steps) => ???
    case ResumeEvaluationStep(b, aOpt, pOpt, steps)         => ???
    case ResumeTopContextStep()                             => ???
    case NoteStep(note)                                     => ???
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(pb, substep.step)
    case YetStep(expr)                          => ???
    case SetFieldsWithIntrinsicsStep(ref, desc) => ???
    case PerformBlockStep(b, d)                 => ???
  }

  /** compile local variable */
  def compile(x: Variable): String = ???

  /** compile references */
  def compile(pb: PolyfillBuilder, ref: Reference): String = ???

  /** compile property references */
  def compile(pb: PolyfillBuilder, ref: PropertyReference): String = ???

  /** compile expressions */
  def compile(pb: PolyfillBuilder, expr: Expression): String = ???

  /** compile mathematical operators */
  def compile(pb: PolyfillBuilder, expr: MathOpExpression): String = ???

  /** compile binary operators */
  def compile(op: BinaryExpressionOperator): String = ???

  /** compile unary operators */
  def compile(op: UnaryExpressionOperator): String = ???

  /** compile literals */
  def compile(pb: PolyfillBuilder, lit: Literal): String = ???

  /** compile bitwise operations */
  def compile(op: BitwiseExpressionOperator): String = ???

  /** compile branch conditions */
  def compile(pb: PolyfillBuilder, cond: Condition): String = ???
}
