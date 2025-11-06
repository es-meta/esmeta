package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.spec.Grammar
import esmeta.fuzzer.synthesizer.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG
import scala.math.*

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
    elapsedBlock: Int,
  ): Seq[Result] = (for {
    (condView, cov) <- target
    CondView(cond, view) = condView
    sites = cov.targetCondViews.getOrElse(cond, Map()).getOrElse(view, Set())
    nearests = sites.collect { case n @ Target.Normal(_, true) => n }
    sources = sites -- nearests
    mutTargets = if (randBool(pow(0.95, elapsedBlock))) nearests else sources
    if mutTargets.nonEmpty
    mutTarget = choose(mutTargets.toVector)
  } yield {
    import Code.*
    code match
      case Normal(codeStr) =>
        mutTarget match
          case Target.Normal(loc, _) =>
            Walker(loc, n)
              .walk(scriptParser.from(codeStr))
              .map(ast =>
                Result(name, Normal(ast.toString(grammar = Some(cfg.grammar)))),
              )
          case _ => List()
      case builtin: Builtin =>
        mutTarget match
          case Target.BuiltinThis(ast) =>
            for {
              ast <- apply(ast, n, target)
              str = ast.toString(grammar = Some(cfg.grammar)).trim
              newCode = mutTarget.updateCode(builtin, str)
            } yield Result(name, newCode)
          case Target.BuiltinArg(ast, _) =>
            for {
              ast <- apply(ast, n, target)
              str = ast.toString(grammar = Some(cfg.grammar)).trim
              newCode = mutTarget.updateCode(builtin, str)
            } yield Result(name, newCode)
          case _ => List()
  }).getOrElse(randomMutator(code, n, target))

  /** mutate ASTs */
  def apply(ast: Ast, n: Int, target: Option[(CondView, Coverage)]): Seq[Ast] =
    randomMutator(ast, n, target)

  /** internal walker for mutating normal target */
  class Walker(loc: Loc, n: Int) extends Util.MultiplicativeListWalker {
    override def walk(ast: Syntactic): List[Syntactic] =
      if (ast.loc == Some(loc)) TotalWalker(ast, n)
      else super.walk(ast)
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
