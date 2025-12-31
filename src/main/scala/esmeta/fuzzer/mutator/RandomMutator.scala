package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.{Walker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*
import esmeta.fuzzer.synthesizer.*
import esmeta.cfg.CFG

/** A random ECMAScript AST mutator */
class RandomMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  import Mutator.*, RandomMutator.*, Code.*

  /** synthesizer */
  val synthesizer = synBuilder(cfg.grammar)

  val names = List("RandomMutator")

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = code match
    case Normal(str) =>
      apply(str, n, target).map(str => Result(name, Normal(str)))
    case builtin @ Builtin(_, _, _, preStmts, postStmts) =>
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
        builtin.mutateArgStr(n, target)
      }

  /** mutate ASTs */
  def apply(ast: Ast, n: Int, target: Option[(CondView, Coverage)]): Seq[Ast] =
    // count of mutation target asts
    val k = targetAstCounter(ast)
    if (k > 0) {
      c = (n - 1) / k + 1
      shuffle(Walker.walk(ast)).take(n)
    } else List.fill(n)(ast)

  /** number of new candidates to make for each target */
  private var c = 0

  /** internal walker */
  object Walker extends Util.AdditiveListWalker {
    override def walk(ast: Syntactic): List[Syntactic] =
      val mutants = super.walk(ast)
      if (isTarget(ast))
        val manuals =
          if (ast.name == "AssignmentExpression")
            val negNums = List("-0.1", "-0", "-1", "-0n", "-1n", "-Infinity")
            val specials = List("NaN", "Symbol()")
            (negNums ++ specials)
              .map(esParser("AssignmentExpression", ast.args).from)
              .map(_.asInstanceOf[Syntactic])
          else Nil
        manuals ++ List.tabulate(c)(_ => synthesizer(ast)) ++ mutants
      else mutants
    override def walk(lex: Lexical): List[Lexical] = lex.name match {
      case "BooleanLiteral" =>
        List("true", "false").map(b => Lexical(lex.name, b))
      case "NumericLiteral" =>
        List("0.1", "0", "1", "0n", "1n").map(n => Lexical(lex.name, n))
      case "StringNumericLiteral" => List(Lexical(lex.name, "Infininty"))
      case _                      => List(lex)
    }
  }
}
object RandomMutator {
  // true if the given ast is target ast
  def isTarget(ast: Ast): Boolean = List(
    "AssignmentExpression",
    "PrimaryExpression",
    "Statement",
    "Declaration",
  ).contains(ast.name)

  // count the number of predefined target asts
  val targetAstCounter = new Util.AstCounter(isTarget)
}
