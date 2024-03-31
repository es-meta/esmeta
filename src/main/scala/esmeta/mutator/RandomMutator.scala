package esmeta.mutator

import esmeta.es.*
import esmeta.es.util.{Walker => AstWalker}
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.mutator.*
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.*
import esmeta.synthesizer.*
import esmeta.cfg.CFG

/** A random ECMAScript AST mutator */
class RandomMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  import RandomMutator.*

  /** synthesizer */
  val synthesizer = synBuilder(cfg.grammar)

  /** default weight for RandomMutator is 3 */
  def calculateWeight(ast: Ast): Int = 3

  val names = List("RandomMutator")

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

  /** internal walker */
  object Walker extends Util.AdditiveListWalker {
    override def walk(ast: Syntactic): List[Syntactic] =
      val mutants = super.walk(ast)
      if isTarget(ast) then List.tabulate(c)(_ => synthesizer(ast)) ++ mutants
      else mutants
    override def walk(lex: Lexical): List[Lexical] = lex.name match {
      case "NumericLiteral" =>
        List("0", "1", "0n", "1n").map(n => Lexical(lex.name, n))
      case "BooleanLiteral" =>
        List("true", "false").map(b => Lexical(lex.name, b))
      case _ => List(lex)
    }
  }
}
object RandomMutator {
  // true if the given ast is target ast
  def isTarget = (ast: Ast) =>
    List(
      "AssignmentExpression",
      "PrimaryExpression",
      "Statement",
      "Declaration",
    )
      .contains(ast.name)

  // count the number of target sub-ast
  val targetAstCounter = new Util.AstCounter(isTarget)
}
