package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG
import esmeta.parser.{ESParser, AstFrom}

/** ECMAScript AST mutator */
trait Mutator(using val cfg: CFG) {
  import Mutator.*

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser
  lazy val scriptParser: AstFrom = esParser("Script")
  lazy val assignExprParser: AstFrom =
    esParser("AssignmentExpression", List(true, false, false))

  /** placeholder for weight */
  val weight: Int

  /** mutate string */
  def apply(code: String): Result = apply(code, 1, None).head
  def apply(code: String, n: Int): Seq[Result] = apply(code, n, None)
  def apply(
    code: String,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = for {
    ast <- apply(scriptParser.from(code), n, target)
    str = ast.toString(grammar = Some(cfg.grammar))
  } yield Result(name, Code.Normal(str))

  /** mutate code */
  def apply(code: Code): Result = apply(code, 1, None).head
  def apply(code: Code, n: Int): Seq[Result] = apply(code, n, None)
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result]

  /** mutate ASTs */
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
}

object Mutator {

  /** Result of mutation with mutator name and AST */
  case class Result(name: String, code: Code)

  extension (target: Target) {

    /** Get the location from normal target */
    def loc: Loc = target match
      case Target.Normal(loc) => loc
      case _                  => raise("target must be normal")

    /** Get the AST from builtin target */
    def ast: Ast = target match
      case Target.BuiltinThis(ast)   => ast
      case Target.BuiltinArg(ast, _) => ast
      case _                         => raise("target must be builtin")

    /** Update the builtin code with the given string at the target position */
    def updateCode(builtin: Code.Builtin, str: String): Code.Builtin =
      target match
        case Target.BuiltinThis(_) => builtin.copy(thisArg = Some(str))
        case Target.BuiltinArg(_, i) =>
          builtin.copy(args = Some(builtin.args.getOrElse(Nil).updated(i, str)))
        case _ => raise("target must be builtin")
  }
}
