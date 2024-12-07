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
  import Mutator.Result

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser
  lazy val scriptParser: AstFrom = esParser("Script")

  /*placeholder for weight*/
  def calculateWeight(ast: Ast): Int

  /** mutate string */
  def apply(code: String): Result = apply(code, 1, None).head
  def apply(code: String, n: Int): Seq[Result] = apply(code, n, None)
  def apply(
    code: String,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = apply(scriptParser.from(code), n, target)

  /** mutate asts */
  def apply(ast: Ast): Result = apply(ast, 1, None).head
  def apply(ast: Ast, n: Int): Seq[Result] = apply(ast, n, None)
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result]

  /** Possible names of underlying mutators */
  val names: List[String]
  lazy val name: String = names.head
}

object Mutator {

  /** Result of mutation */
  case class Result(code: String, ast: Ast)
}
