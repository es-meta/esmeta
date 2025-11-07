package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG
import esmeta.parser.{ESParser, AstFrom}

/** ECMAScript AST mutator */
trait Mutator(using val cfg: CFG) {
  import Mutator.*, Code.*

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser
  lazy val scriptParser: AstFrom = esParser("Script")
  lazy val argumentListParser: AstFrom =
    esParser("ArgumentList", List(false, false))

  /** mutate code */
  def apply(code: Code): Result = apply(code, 1, None).head
  def apply(code: Code, n: Int): Seq[Result] = apply(code, n, None)
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result]

  /** mutate string */
  def apply(str: String): String = apply(str, 1, None).head
  def apply(str: String, n: Int): Seq[String] = apply(str, n, None)
  def apply(
    str: String,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[String] = apply(scriptParser.from(str), n, target).map { ast =>
    ast.toString(grammar = Some(cfg.grammar))
  }

  /** mutate AST */
  def apply(ast: Ast): Ast = apply(ast, 1, None).head
  def apply(ast: Ast, n: Int): Seq[Ast] = apply(ast, n, None)
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Ast]

  /** Possible names of underlying mutators */
  val names: List[String]
  lazy val name: String = names.head

  /** helper for mutation in builtin code */
  extension (builtin: Builtin) {
    def mutatePreStmts(
      n: Int,
      target: Option[(CondView, Coverage)],
    ): Seq[Result] = for {
      preStmts <- builtin.preStmts.toSeq
      mutatedStmts <- apply(preStmts, n, target)
    } yield Result(name, builtin.copy(preStmts = Some(mutatedStmts)))
    def mutatePostStmts(
      n: Int,
      target: Option[(CondView, Coverage)],
    ): Seq[Result] = for {
      postStmts <- builtin.postStmts.toSeq
      mutatedStmts <- apply(postStmts, n, target)
    } yield Result(name, builtin.copy(postStmts = Some(mutatedStmts)))
    def mutateArgStr(
      n: Int,
      target: Option[(CondView, Coverage)],
    ): Seq[Result] =
      if (builtin.targetArgs.nonEmpty)
        val mutationCite = choose(builtin.targetArgs)
        val argStr = mutationCite.argStr
        for {
          mutatedAst <- apply(argumentListParser.from(argStr), n, target)
          mutatedStr = mutatedAst.toString(grammar = Some(cfg.grammar))
          mutatedCode = builtin.replace(mutationCite, mutatedStr)
        } yield Result(name, mutatedCode)
      else List.fill(n)(builtin).map(Result(name, _))
  }
}

object Mutator {
  import Code.*, Target.*

  /** Result of mutation with mutator name and code */
  case class Result(name: String, code: Code)

  /** Update the builtin code with the given string at the target position */
  extension (builtin: Builtin) {
    def replace(target: Target, str: String): Builtin = target match
      case BuiltinThis(_)   => builtin.copy(thisArg = Some(str))
      case BuiltinArg(_, i) => builtin.copy(args = builtin.args.updated(i, str))
      case _                => raise("target must be builtin")
  }
}
