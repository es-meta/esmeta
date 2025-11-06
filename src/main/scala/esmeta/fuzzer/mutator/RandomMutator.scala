package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.{Walker => AstWalker}
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.*
import esmeta.fuzzer.synthesizer.*
import esmeta.cfg.CFG

/** A random ECMAScript AST mutator */
class RandomMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  import RandomMutator.*
  import Mutator.*

  /** synthesizer */
  val synthesizer = synBuilder(cfg.grammar)

  /** default weight for RandomMutator is 3 */
  val weight: Int = 3

  val names = List("RandomMutator")

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
    elapsedBlock: Int,
  ): Seq[Result] = code match
    case Code.Normal(str) => apply(str, n, target)
    case builtin: Code.Builtin =>
      val mutTargets = Target(builtin)(using assignExprParser)
      if (mutTargets.isEmpty) Nil
      else
        import Target.*
        val mutTarget = choose(mutTargets.toVector)
        for {
          ast <- this.apply(mutTarget.ast, n, target)
          str = ast.toString(grammar = Some(cfg.grammar)).trim
          newCode = mutTarget.updateCode(builtin, str)
        } yield Result(name, newCode)

  /** mutate ASTs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Ast] =
    val k = targetAstCounter(ast)
    if (k > 0) c = (n - 1) / k + 1
    shuffle(Walker.walk(ast)).take(n)

  /* number of new candidates to make for each target */
  var c = 0

  /** internal walker */
  object Walker extends Util.AdditiveListWalker {
    override def walk(ast: Syntactic): List[Syntactic] =
      val mutants = super.walk(ast)
      val assignExprParser = esParser("AssignmentExpression", ast.args)
      if (isTarget(ast))
        val manuals =
          if (ast.name == "AssignmentExpression")
            List(
              assignExprParser.from("-0.1"),
              assignExprParser.from("-0"),
              assignExprParser.from("-1"),
              assignExprParser.from("-0n"),
              assignExprParser.from("-1n"),
              assignExprParser.from("-Infinity"),
              assignExprParser.from("NaN"),
              assignExprParser.from("Symbol()"),
            ).map(_.asInstanceOf[Syntactic])
          else List()
        manuals ++ List.tabulate(c)(_ => synthesizer(ast)) ++ mutants
      else mutants
    override def walk(lex: Lexical): List[Lexical] = lex.name match {
      case "BooleanLiteral" =>
        List("true", "false").map(b => Lexical(lex.name, b))
      case "NumericLiteral" =>
        List("0.1", "0", "1", "0n", "1n").map(n => Lexical(lex.name, n))
      case "StringNumericLiteral" =>
        List("Infinity").map(sn => Lexical(lex.name, sn))
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
