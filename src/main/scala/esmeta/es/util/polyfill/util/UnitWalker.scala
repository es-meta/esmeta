package esmeta.es.util.polyfill.util

import esmeta.es.util.polyfill.{PolyfillExpr, PolyfillStep}
import esmeta.lang.util.UnitWalker as LangUnitWalker

/** a unit walker for the polyfill pipeline language
  *
  * The counterpart of [[Walker]] for traversals that only collect, such as
  * scanning a rewritten body for the variables it mentions.
  */
trait UnitWalker extends LangUnitWalker {
  import PolyfillStep.*
  import PolyfillExpr.*

  def walk(step: PolyfillStep): Unit = step match {
    case LangStep(step) => walk(step)
    case Steps(steps)   => walkList(steps, walk)
    case Branch(cond, thenStep, elseStep, _) =>
      walk(cond); walk(thenStep); walkOpt(elseStep, walk)
    case Loop(header, body)  => walk(header); walk(body)
    case Let(variable, expr) => walk(variable); walk(expr)
    case Assign(ref, expr)   => walk(ref); walk(expr)
    case Perform(expr)       => walk(expr)
    case Return(expr)        => walk(expr)
    case Rethrow(_)          => ()
    case CompletionCheck(_, _, _, cond, t, e, _) =>
      walk(cond); walk(t); walkOpt(e, walk)
    case Wrapped(tryBlock, catchVar, catchBlock) =>
      walk(tryBlock); walk(catchVar); walkOpt(catchBlock, walk)
  }

  def walk(expr: PolyfillExpr): Unit = expr match {
    case LangExpr(expr) => walk(expr)
    case Closure(params, captured, body) =>
      walkList(params, walk); walkList(captured, walk); walk(body)
    case Invoke(_, args, _) => walkList(args, walk)
  }
}
