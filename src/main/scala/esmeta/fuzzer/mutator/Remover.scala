package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*
import esmeta.fuzzer.synthesizer.*
import esmeta.cfg.CFG

/** A mutator that removes nodes of ECMAScript AST */
class Remover(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator
  with Util.MultiplicativeListWalker {
  import Mutator.*, Remover.*, Code.*

  val randomMutator = RandomMutator()

  val names = "Remover" :: randomMutator.names

  val synthesizer = synBuilder(cfg.grammar)

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = code match
    case Normal(str) =>
      apply(str, n, target).map(str => Result(name, Normal(str)))
    case builtin @ Builtin(_, thisArg, args, preStmts, postStmts) =>
      if ((preStmts.isDefined || postStmts.isDefined) && randBool) {
        // mutate statements
        (preStmts, postStmts) match
          case (Some(_), Some(_)) =>
            if randBool then builtin.mutatePreStmts(n, target)
            else builtin.mutatePostStmts(n, target)
          case (Some(_), None) => builtin.mutatePreStmts(n, target)
          case (None, Some(_)) => builtin.mutatePostStmts(n, target)
          case (None, None)    => raise("unreachable")
      } else {
        // mutate builtin call arguments
        if (args.isEmpty && thisArg.isEmpty) randomMutator(builtin, n, target)
        else {
          (0 to args.length).flatMap(args.combinations).flatMap { args =>
            Result(name, builtin.copy(args = args)) :: thisArg.toList.map { _ =>
              Result(name, builtin.copy(thisArg = None, args = args))
            }
          }
        }
      }

  /** mutate ASTs */
  def apply(ast: Ast, n: Int, target: Option[(CondView, Coverage)]): Seq[Ast] =
    // count of removal candidates
    val k = victimCounter(ast)
    if (k > 0) {
      k1 = 0; k2 = k
      // if n is bigger than 2^k (the total size of the search space),
      // fill the remaining count with the randomly generated program.
      if (Math.pow(2, k) < n) {
        walk(ast) ++ randomMutator(ast, n - (1 << k), target)
      } else {
        // calculate the most efficient parameters
        // until 2^(k2 - 1) < n, increase k1 and decrease k2 (initially k)
        // if we have 5 victims and n is 4, k1 = 2, k2 = 3 after this loop.
        // k1: the number of survivors among victims
        // k2: the number of casualties among victims
        while (Math.pow(2, k2 - 1) >= n) { k1 += 1; k2 -= 1 }
        shuffle(walk(ast)).take(n)
      }
    } else randomMutator(ast, n, target)

  /** parameter for sampler */
  private var (k1, k2) = (0, 0)

  private def doDrop: Boolean =
    val prob: Float = k1.toFloat / (k1 + k2)
    if (k1 > 0 && randBool(prob)) { k1 -= 1; false }
    else if (k2 > 0) { k2 -= 1; true }
    else throw new Error("This is a bug in Remover")

  /** ast walker */
  override def walk(ast: Syntactic): List[Syntactic] =
    val mutants = super.walk(ast)
    val i = findSameChild(ast)
    if (i >= 0 && doDrop)
      mutants ++ mutants.map(_.children(i).get.asInstanceOf[Syntactic])
    else mutants
}

object Remover {
  // Refactor: Was it written properly?
  def findSameChild(ast: Ast): Int = ast match
    case Syntactic(name, args, rhsIdx, children) =>
      children.indexWhere(_ match
        case Some(Syntactic(`name`, `args`, _, _)) => true
        case _                                     => false,
      )
    case _ => -1

  // count the number of asts that have same child
  val victimCounter = Util.AstCounter(ast => findSameChild(ast) >= 0)
}
