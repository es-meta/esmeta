package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.fuzzer.synthesizer.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG

/** A target ECMAScript AST mutator */
class TargetMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  import Mutator.*, Code.*

  val randomMutator = RandomMutator()

  val names = "TargetMutator" :: randomMutator.names

  /** synthesizer */
  val synthesizer = synBuilder(cfg.grammar)

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = (for {
    (cv, cov) <- target
    CondView(cond, view) = cv
    targets = cov.targetCondViews.getOrElse(cond, Map()).getOrElse(view, Set())
    normalTargets = targets.collect { case normal: Target.Normal => normal }
    builtinTargets = targets.collect {
      case builtinThis: Target.BuiltinThis => builtinThis
      case builtinArg: Target.BuiltinArg   => builtinArg
    }
  } yield {
    code match
      case Normal(str) =>
        if (normalTargets.nonEmpty) {
          val mutationCite = choose(normalTargets)
          Walker(mutationCite, n)
            .walk(scriptParser.from(str))
            .map(_.toString(grammar = Some(cfg.grammar)))
            .map(str => Result(name, Normal(str)))
        } else apply(str, n, target).map(str => Result(name, Normal(str)))
      case builtin: Builtin =>
        if (builtinTargets.nonEmpty) {
          val mutationCite = choose(builtinTargets)
          val argStr = mutationCite.argStr
          for {
            mutatedAst <- apply(argumentListParser.from(argStr), n, target)
            mutatedStr = mutatedAst.toString(grammar = Some(cfg.grammar))
            mutatedCode = builtin.replace(mutationCite, mutatedStr)
          } yield Result(name, mutatedCode)
        } else randomMutator(builtin, n, target)
  }).getOrElse(randomMutator(code, n, target))

  /** mutate ASTs */
  def apply(ast: Ast, n: Int, target: Option[(CondView, Coverage)]): Seq[Ast] =
    randomMutator(ast, n, target)

  /** internal walker for mutating normal target */
  class Walker(target: Target.Normal, n: Int)
    extends Util.MultiplicativeListWalker {
    override def walk(ast: Syntactic): List[Syntactic] =
      if ast.loc == Some(target.loc) then TotalWalker(ast, n)
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
