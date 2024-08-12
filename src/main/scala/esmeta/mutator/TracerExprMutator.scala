package esmeta.mutator

import esmeta.cfg.CFG
import esmeta.es.Ast
import esmeta.es.util.Coverage.CondView
import esmeta.es.util.Coverage
import esmeta.es.Syntactic
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.shuffle
import esmeta.es.util.fuzzer.TRACER_SYMBOL

class TracerExprMutator(using cfg: CFG) extends Mutator {
  import TracerExprMutator.*

  val grammar: Grammar = cfg.grammar

  val names: List[String] = List("TracerExprMutator")

  def calculateWeight(ast: Ast): Int = 3

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] =
    val k = targetAstCounter(ast)
    if (k == 0)
      List.fill(n)(ast)
    else
      c = (n - 1) / k + 1
    shuffle(Walker.walk(ast)).take(n).map((name, _))

  /* number of new candidates to make for each target */
  var c = 0

  object Walker extends Util.AdditiveListWalker {
    override def walk(syn: Syntactic): List[Syntactic] = syn match {
      case Syntactic("Expression", args, 0, children) =>
        val candidates = walkVector(children, walkOpt)
        for (candidate <- candidates) yield {
          val expr = Syntactic("Expression", args, 0, candidate)
            .toString(grammar = Some(grammar))
          val syn = esParser("Expression", args)
            .from(s"$TRACER_SYMBOL($expr)")
            .asInstanceOf[Syntactic]
          syn
        }
      case Syntactic("Expression", args, 1, children) =>
        val candidates = walkVector(children, walkOpt)
        for (candidate <- candidates) yield {
          val expr = Syntactic("Expression", args, 1, candidate)
            .toString(grammar = Some(grammar))
          val syn = esParser("Expression", args)
            .from(s"$TRACER_SYMBOL(($expr))")
            .asInstanceOf[Syntactic]
          syn
        }
      case _ => super.walk(syn)
    }
  }
}
object TracerExprMutator {
  // true if the given ast is target ast
  def isTarget = (ast: Ast) => List("Expression").contains(ast.name)

  // count the number of target sub-ast
  val targetAstCounter = new Util.AstCounter(isTarget)
}
