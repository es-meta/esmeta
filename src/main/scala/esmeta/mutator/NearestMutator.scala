package esmeta.mutator

import esmeta.es.*
import esmeta.state.*
import esmeta.spec.Grammar
import esmeta.synthesizer.*
import esmeta.ty.AstSingleTy
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG

/** A nearest ECMAScript AST mutator */
class NearestMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  import NearestMutator.*

  val names = "NearestMutator" :: randomMutator.names

  /** synthesizer */
  val synthesizer = synBuilder(cfg.grammar)

  val randomMutator = RandomMutator()

  /** default weight for NearestMutator is 6 */
  def calculateWeight(ast: Ast): Int = 6

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] = (for {
    (condView, cov) <- target
    CondView(cond, view) = condView
    nearest <- cov.targetCondViews.getOrElse(cond, Map()).getOrElse(view, None)
  } yield Walker(nearest, n).walk(ast).map((name, _)))
    .getOrElse(randomMutator(ast, n, target))

  /** internal walker */
  class Walker(nearest: Nearest, n: Int) extends Util.MultiplicativeListWalker {
    val AstSingleTy(name, rhsIdx, subIdx) = nearest.ty
    override def walk(ast: Syntactic): List[Syntactic] =
      if (
        ast.name == name &&
        ast.rhsIdx == rhsIdx &&
        ast.subIdx == subIdx &&
        ast.loc == Some(nearest.loc)
      )
        TotalWalker(ast, n)
      else
        super.walk(ast)
  }

  /** internal walker that mutates all internal nodes with same prob. */
  object TotalWalker extends Util.AdditiveListWalker {
    var c = 0
    def apply(ast: Syntactic, n: Int): List[Syntactic] =
      val k = Util.simpleAstCounter(ast)
      c = (n - 1) / k + 1
      shuffle(walk(ast)).take(n).toList

    override def walk(ast: Syntactic): List[Syntactic] =
      val mutants = super.walk(ast)
      List.tabulate(c)(_ => synthesizer(ast)) ++ mutants
  }
}
