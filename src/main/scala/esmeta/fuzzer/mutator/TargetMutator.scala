package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.state.{Target => StateTarget}
import esmeta.spec.Grammar
import esmeta.fuzzer.synthesizer.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG

/** A target ECMAScript AST mutator */
class TargetMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  import Mutator.*

  val randomMutator = RandomMutator()

  val names = "TargetMutator" :: randomMutator.names

  /** synthesizer */
  val synthesizer = synBuilder(cfg.grammar)

  /** default weight for TargetMutator is 6 */
  val weight: Int = 6

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = code match
    case Code.Normal(str) => apply(str, n, target)
    case _: Code.Builtin  => Nil // TODO

  /** mutate ASTs */
  def apply(
    ast: Ast,
    n: Int,
    targetBranch: Option[(CondView, Coverage)],
  ): Seq[Ast] = (for {
    (condView, cov) <- targetBranch
    CondView(cond, view) = condView
    targets = cov.targetCondViews.getOrElse(cond, Map()).getOrElse(view, Set())
    if targets.nonEmpty
    target = choose(targets.toVector)
  } yield Walker(target, n).walk(ast))
    .getOrElse(randomMutator(ast, n, targetBranch))

  /** internal walker */
  class Walker(target: StateTarget, n: Int)
    extends Util.MultiplicativeListWalker {
    val StateTarget(name, rhsIdx, subIdx, loc) = target
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
