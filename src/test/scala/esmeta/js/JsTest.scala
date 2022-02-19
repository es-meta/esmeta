package esmeta.js

import esmeta.ESMetaTest
import esmeta.spec.Spec
import esmeta.js.util.*
import esmeta.util.SystemUtils.*

trait JSTest extends ESMetaTest {
  def category: String = "js"

  def spec = ESMetaTest.spec
  def grammar = spec.grammar

  // tests for js parser
  lazy val parser = Parser(grammar)("Script")
  def jsParseTest(ast: Ast): Ast =
    val newAst = parser.from(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def jsParseTest(str: String): Ast =
    jsParseTest(parser.from(str))
  def jsParseTestFile(filename: String): Ast =
    jsParseTest(parser.fromFile(filename))
}
