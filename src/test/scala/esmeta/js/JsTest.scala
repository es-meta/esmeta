package esmeta.js

import esmeta.ESMetaTest
import esmeta.spec.Spec
import esmeta.js.util.*
import esmeta.util.SystemUtils.*

trait JSTest extends ESMetaTest {
  def category: String = "js"

  // tests for js parser
  lazy val parser = Parser(ESMetaTest.spec.grammar).parser("Script")
  def jsParseTest(ast: Ast): Ast = {
    val newAst = parser(ast.toString(grammar = Some(ESMetaTest.spec.grammar)))
    assert(ast == newAst)
    ast
  }
  def jsParseTest(str: String): Ast = jsParseTest(parser(str))
  def jsParseTestFile(filename: String): Ast =
    val content = readFile(filename)
    jsParseTest(content)
}
