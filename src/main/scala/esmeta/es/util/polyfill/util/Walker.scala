package esmeta.es.util.polyfill.util

import esmeta.es.util.polyfill.{PolyfillExpr, PolyfillStep}
import esmeta.lang.util.Walker as LangWalker

/** a walker for the polyfill pipeline language
  *
  * Extends the metalanguage walker, so a rule that only cares about the
  * expressions or references inside a step overrides the inherited
  * [[esmeta.lang.util.Walker]] methods and gets the pipeline structure
  * traversed for free.
  */
trait Walker extends LangWalker {
  import PolyfillStep.*
  import PolyfillExpr.*

  def walk(step: PolyfillStep): PolyfillStep = step match {
    case LangStep(step) => LangStep(walk(step))
    case Steps(steps)   => Steps(walkList(steps, walk))
    case Branch(cond, thenStep, elseStep, elseConfig) =>
      Branch(walk(cond), walk(thenStep), walkOpt(elseStep, walk), elseConfig)
    case Loop(header, body)  => Loop(walk(header), walk(body))
    case Let(variable, expr) => Let(walk(variable), walk(expr))
    case Assign(ref, expr)   => Assign(walk(ref), walk(expr))
    case Perform(expr)       => Perform(walk(expr))
    case Return(expr)        => Return(walk(expr))
    case Rethrow(name)       => Rethrow(name)
    case CompletionCheck(v, ct, f, cond, t, e, cfg) =>
      CompletionCheck(v, ct, f, walk(cond), walk(t), walkOpt(e, walk), cfg)
    case Wrapped(tryBlock, catchVar, catchBlock) =>
      Wrapped(walk(tryBlock), walk(catchVar), walkOpt(catchBlock, walk))
  }

  def walk(expr: PolyfillExpr): PolyfillExpr = expr match {
    case LangExpr(expr) => LangExpr(walk(expr))
    case Closure(params, captured, body) =>
      Closure(walkList(params, walk), walkList(captured, walk), walk(body))
    case Invoke(name, args, tag) => Invoke(name, walkList(args, walk), tag)
  }
}
