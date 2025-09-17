package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.parser.{ESParser, AstFrom}

/** ECMAScript AST mutator */
trait Mutator(using val cfg: CFG) {
  import Mutator.*

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser
  lazy val scriptParser: AstFrom = esParser("Script")
  lazy val exprParser: AstFrom = esParser("AssignmentExpression")

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

  extension (builtin: Code.Builtin) {
    def toTargets: List[Target] =
      import Target.*
      val args = for {
        (arg, idx) <- builtin.args.getOrElse(Nil).zipWithIndex
        ast = exprParser.from(arg)
      } yield Arg(idx, ast)
      builtin.thisArg.fold(args)(arg => This(exprParser.from(arg)) :: args)
  }
}

object Mutator {

  /** Result of mutation with mutator name and AST */
  case class Result(name: String, code: Code)

  /** mutation target */
  enum Target {
    case This(ast: Ast)
    case Arg(idx: Int, ast: Ast)
    val ast: Ast

    import Code.*
    lazy val updateCode: (Builtin, String) => Builtin = this match
      case This(_) => (b, str) => b.copy(thisArg = Some(str))
      case Arg(idx, _) =>
        (b, str) => b.copy(args = Some(b.args.getOrElse(Nil).updated(idx, str)))
  }
}
