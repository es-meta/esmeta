package esmeta.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.parser.{ESParser, AstFrom}

/** ECMAScript AST mutator */
trait Mutator(using val cfg: CFG) {

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser
  lazy val scriptParser: AstFrom = esParser("Script")

  private type Result = Seq[(String, Ast)]

  /*placeholder for weight*/
  def calculateWeight(ast: Ast): Int

  /** mutate string */
  def apply(code: String, n: Int): Result =
    apply(code, n, None)
  def apply(
    code: String,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Result = apply(scriptParser.from(code), n, target)

  /** mutate asts */
  def apply(ast: Ast, n: Int): Result = apply(ast, n, None)
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Result

  /** Possible names of underlying mutators */
  val names: List[String]
  lazy val name: String = names.head
}
