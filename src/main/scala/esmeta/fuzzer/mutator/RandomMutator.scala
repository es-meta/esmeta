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

  val names = List("RandomMutator")

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] =
    val k = targetAstCounter(ast)
    if (k == 0)
      List.fill(n)(ast)
    else
      c = (n - 1) / k + 1
    shuffle(Walker.walk(ast)).take(n).map(Result(name, _))

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
