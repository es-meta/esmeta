package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.state.*
import esmeta.spec.Grammar
import esmeta.fuzzer.synthesizer.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG
// import esmeta.analyzer.paramflow.*

/** A nearest ECMAScript AST mutator */
class NearestMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  import NearestMutator.*
  import Mutator.*

  /** analysis to guide mutation */
  // private lazy val analyzer = ParamFlowAnalyzer(cfg)
  // import analyzer.*
  // analyze

  val randomMutator = RandomMutator()

  val names = "NearestMutator" :: randomMutator.names

  /** synthesizer */
  val synthesizer = synBuilder(cfg.grammar)

  /** default weight for NearestMutator is 6 */
  def calculateWeight(ast: Ast): Int = 6

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = (for {
    (condView, cov) <- target
    CondView(cond, view) = condView
    // curNp = NodePoint(cfg.funcOf(cond.branch), cond.branch, emptyView)
    // st = getResult(curNp)
    // (v, _) = transfer.transfer(cond.branch.cond)(using curNp)(st)
    // // FIXME: debug
    // _debug =
    //   println(
    //     s"${cond.id}: ${cond.branch.cond} @ ${cfg.funcOf(cond.branch).name}",
    //   )
    //   println(s"affected by $v")
    //   println()
    // // TODO: link param to exact AST
    nearest <- cov.targetCondViews.getOrElse(cond, Map()).getOrElse(view, None)
  } yield Walker(nearest, n).walk(ast).map(Result(name, _)))
    .getOrElse(randomMutator(ast, n, target))

  /** internal walker */
  class Walker(nearest: Nearest, n: Int) extends Util.MultiplicativeListWalker {
    val Nearest(name, rhsIdx, subIdx, loc) = nearest
    override def walk(ast: Syntactic): List[Syntactic] =
      if (
        ast.name == name &&
        ast.rhsIdx == rhsIdx &&
        ast.subIdx == subIdx &&
        ast.loc == Some(loc)
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
