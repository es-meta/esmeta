package esmeta.js

import esmeta.ESMetaTest
import esmeta.spec.Spec
import esmeta.interp.*
import esmeta.ir.NormalInst
import esmeta.js.util.*
import esmeta.util.SystemUtils.*
import org.scalatest.Assertions.*

trait JSTest extends ESMetaTest {
  def category: String = "js"
}
object JSTest {
  lazy val spec = ESMetaTest.spec
  lazy val grammar = spec.grammar
  lazy val cfg = spec.toCFG

  // file extension converter from .js to .ir
  lazy val js2ir = changeExt("js", "ir")

  // parse JS codes
  lazy val parser = Parser(grammar)("Script")
  def parse(str: String): Ast = parser.from(str)
  def parseFile(filename: String): Ast = parser.fromFile(filename)

  // eval JS codes
  def eval(
    str: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State =
    Interp(Initialize(cfg, str, cachedAst))
  def evalFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State = eval(readFile(filename), checkAfter, cachedAst)

  // tests for JS parser
  def parseTest(ast: Ast): Ast =
    val newAst = parser.from(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast = parseTest(parse(str))
  def parseFileTest(filename: String): Ast = parseTest(parseFile(filename))

  // tests for JS interpreter
  def checkExit(st: State): st.type = st(GLOBAL_RESULT) match
    case comp: Comp => assert(comp.ty == CONST_NORMAL); st
    case v          => fail(s"return not a completion: $v")
  def evalTest(
    str: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State = checkExit(eval(str, checkAfter, cachedAst))
  def evalTestFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State = checkExit(evalFile(filename, checkAfter, cachedAst))
}
