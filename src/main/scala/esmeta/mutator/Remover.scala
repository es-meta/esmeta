package esmeta.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*
import esmeta.spec.Grammar
import esmeta.synthesizer.*
import esmeta.cfg.CFG

/** A mutator that removes nodes of ECMAScript AST */
class Remover(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator
  with Util.MultiplicativeListWalker {
  import Remover.*

  val randomMutator = RandomMutator()

  val names = "Remover" :: randomMutator.names

  val synthesizer = synBuilder(cfg.grammar)

  /** default weight for Remover is 1 */
  def calculateWeight(ast: Ast): Int = 1

  /** mutate a program */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] = {
    // count of removal candidates
    val k = victimCounter(ast)
    if (k == 0) randomMutator(ast, n, target)
    else
      k1 = 0; k2 = k
      // if n is bigger than 2^k (the total size of the search space),
      // fill the remaining count with the randomly generated program.
      if (Math.pow(2, k) < n)
        walk(ast)
          .map((name, _)) ++ randomMutator(ast, n - (1 << k), target)
      else {
        // calculate the most efficient parameters
        // until 2^(k2 - 1) < n, increase k1 and decrease k2 (initially k)
        // if we have 5 victims and n is 4, k1 = 2, k2 = 3 after this loop.
        // k1: the number of survivors among victims
        // k2: the number of casualties among victims
        while (Math.pow(2, k2 - 1) >= n)
          k1 = k1 + 1
          k2 = k2 - 1
        sample(ast, n)
      }
  }

  /** parameter for sampler */
  private var (k1, k2) = (0, 0)

  private def sample(ast: Ast, n: Int) =
    shuffle(walk(ast)).take(n).map((name, _))

  private def doDrop: Boolean =
    if k1 > 0 && randBool(k1 / (k1 + k2).toFloat) then
      k1 -= 1; false
    else if k2 > 0 then
      k2 -= 1; true
    else throw new Error("This is a bug in Remover")

  /** ast walker */
  override def walk(ast: Syntactic): List[Syntactic] =
    val mutants = super.walk(ast)
    val i = findSameChild(ast)
    if i >= 0 && doDrop then
      mutants ++ mutants.map(_.children(i).get.asInstanceOf[Syntactic])
    else mutants
}

object Remover {
  def findSameChild(ast: Ast): Int = ast match {
    case Syntactic(name, args, rhsIdx, children) =>
      children.indexWhere(_ match {
        case Some(Syntactic(`name`, `args`, _, _)) => true
        case _                                     => false
      })
    case _ => -1
  }

  // count the number of asts that have same child
  val victimCounter = Util.AstCounter(ast => findSameChild(ast) >= 0)
}
