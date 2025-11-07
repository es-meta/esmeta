package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.spec.Grammar
import esmeta.fuzzer.synthesizer.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG

/** An ad-hoc ECMAScript AST mutator */
class AdHocMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  import Mutator.*, Code.*

  val randomMutator = RandomMutator()

  val names = "AdHocMutator" :: randomMutator.names

  /** synthesizer */
  val synthesizer = synBuilder(cfg.grammar)

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = code match
    case Normal(str)      => Nil // TODO
    case builtin: Builtin => Nil // TODO

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
