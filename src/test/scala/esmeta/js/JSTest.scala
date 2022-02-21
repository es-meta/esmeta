package esmeta.js

import esmeta.ESMetaTest
import esmeta.spec.Spec
import esmeta.js.util.*
import esmeta.util.SystemUtils.*

trait JSTest extends ESMetaTest {
  def category: String = "js"
}
object JSTest {
  def spec = ESMetaTest.spec
  def grammar = spec.grammar

  // tests for js parser
  lazy val parser = Parser(grammar)("Script")
  def parseTest(ast: Ast): Ast =
    val newAst = parser.from(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast =
    parseTest(parser.from(str))
  def parseFileTest(filename: String): Ast =
    parseTest(parser.fromFile(filename))
}
