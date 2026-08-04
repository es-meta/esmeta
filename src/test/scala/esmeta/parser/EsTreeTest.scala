package esmeta.parser

import esmeta.ESMetaTest
import esmeta.es.Ast
import esmeta.es.util.AstDiff
import esmeta.parser.estree.{EsTreeParser, FastParser}
import org.scalatest.Assertions.*

/** tests of the ESTree-based ECMAScript parser */
trait EsTreeTest extends ESMetaTest {
  def category: String = "parser"
}
object EsTreeTest {
  import ESMetaTest.*

  /** the reference parser, i.e. the grammar of ECMA-262 */
  lazy val slowParser: AstFrom = spec.esParser("Script")
  lazy val slowModuleParser: AstFrom = spec.esParser("Module")

  /** the parser under test, without the fallback that hides its failures */
  private lazy val fast = FastParser(grammar, fallback = false)
  lazy val fastParser: AstFrom = fast("Script")
  lazy val fastModuleParser: AstFrom = fast("Module")

  /** whether Node.js is available to run the ESTree parser */
  lazy val canUse: Boolean = EsTreeParser.canUse

  /** check that both parsers accept a program and agree on its AST */
  def sameAst(code: String): Unit = sameAst(code, slowParser, fastParser)

  /** the same, with the `Module` goal symbol */
  def sameModuleAst(code: String): Unit =
    sameAst(code, slowModuleParser, fastModuleParser)

  private def sameAst(code: String, slow: AstFrom, fast: AstFrom): Unit =
    val expect =
      try slow.from(code)
      catch {
        case e: Throwable =>
          fail(s"the reference parser failed: ${e.getMessage}")
      }
    val actual =
      try fast.from(code)
      catch {
        case e: Throwable => fail(s"the ESTree parser failed: ${e.getMessage}")
      }
    for (reason <- AstDiff.parentLinks(actual)) fail(reason)
    AstDiff(expect, actual) match
      case None         => ()
      case Some(reason) => fail(reason)

  /** check that the parsers agree on a program, if both accept it
    *
    * A program that the ESTree parser rejects is not a failure: it also checks
    * the early errors of the specification, which ESMeta evaluates on the AST
    * instead, so [[FastParser]] falls back to the reference parser for those.
    */
  def sameAstIfParsed(code: String): Unit =
    val expect =
      try slowParser.from(code)
      catch {
        case _: Throwable => return // not a program of the grammar
      }
    val actual =
      try fastParser.from(code)
      catch {
        case _: Throwable => return // rejected by the early errors of ESTree
      }
    for (reason <- AstDiff.parentLinks(actual)) fail(reason)
    AstDiff(expect, actual) match
      case None         => ()
      case Some(reason) => fail(reason)

  def sameAstFile(filename: String): Unit =
    sameAstIfParsed(readFile(filename))

  private def readFile(filename: String): String =
    esmeta.util.SystemUtils.readFile(filename)
}
