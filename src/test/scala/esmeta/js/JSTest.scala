package esmeta.js

import esmeta.ESMetaTest
import esmeta.analyzer.{AbsSemantics, YET_THROW}
import esmeta.analyzer.domain.*
import esmeta.interp.*
import esmeta.ir.NormalInst
import esmeta.js.util.*
import esmeta.spec.Spec
import esmeta.util.SystemUtils.*
import org.scalatest.Assertions.*

trait JSTest extends ESMetaTest {
  def category: String = "js"
}
object JSTest {
  val spec = ESMetaTest.spec
  val grammar = spec.grammar
  val cfg = {
    val res = spec.toCFG
    YET_THROW = true
    _cfgOpt = Some(res) // initialize global cfg for abstract domain
    res
  }

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
    Interp(Initialize(cfg, str, cachedAst), checkAfter)
  def evalFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State = eval(readFile(filename), checkAfter, cachedAst)

  // analyze JS codes
  def analyzeFile(filename: String): AbsSemantics = {
    val str = readFile(filename)
    analyze(str)
  }
  def analyze(str: String): AbsSemantics = AbsSemantics(str).fixpoint

  // tests for JS parser
  def parseTest(ast: Ast): Ast =
    val newAst = parser.from(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast = parseTest(parse(str))
  def parseFileTest(filename: String): Ast = parseTest(parseFile(filename))

  // tests for JS interpreter
  def checkExit(st: State): st.type = st(GLOBAL_RESULT) match
    case Undef => st
    case v     => fail(s"return not undefined: $v")
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

  // tests for JS analyzer
  def checkExit(absSem: AbsSemantics): AbsSemantics =
    assert(absSem.finalResult.value.getSingle == FlatElem(ASimple(Undef)))
    absSem
  def analyzeTest(str: String): AbsSemantics = checkExit(analyze(str))
  def analyzeTestFile(filename: String): AbsSemantics =
    checkExit(analyzeFile(filename))
}
