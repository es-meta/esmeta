package esmeta.es

import esmeta.ESMetaTest
import esmeta.analyzer.AbsSemantics
import esmeta.analyzer.domain.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.es.util.*
import esmeta.interpreter.*
import esmeta.ir.NormalInst
import esmeta.parser.AstFrom
import esmeta.spec.Spec
import esmeta.state.*
import esmeta.util.SystemUtils.*
import org.scalatest.Assertions.*

/** ECMAScript test */
trait ESTest extends ESMetaTest {
  def category: String = "es"
}
object ESTest {
  import ESMetaTest.*

  // file extension converter from .js to .ir
  lazy val js2ir = changeExt("js", "ir")

  // parse ES codes
  lazy val scriptParser: AstFrom = spec.scriptParser
  def parse(str: String): Ast = scriptParser.from(str)
  def parseFile(filename: String): Ast = scriptParser.fromFile(filename)

  // interpreter with additional assertion checks
  class CheckAfter(
    st: State,
    checkAfter: List[NormalInst],
  ) extends Interpreter(st):
    override lazy val result: State =
      while (step) {}
      for (assert <- checkAfter) super.eval(assert)
      st

  // eval ES codes
  def eval(
    str: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State =
    new CheckAfter(Initialize(cfg, str, cachedAst), checkAfter).result
  def evalFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State = eval(readFile(filename), checkAfter, cachedAst)

  // analyze ES codes
  def analyzeFile(filename: String): AbsSemantics = {
    val str = readFile(filename)
    analyze(str)
  }
  def analyze(str: String): AbsSemantics = AbsSemantics(str).fixpoint

  // tests for ES parser
  def parseTest(ast: Ast): Ast =
    val newAst = parse(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast = parseTest(parse(str))
  def parseFileTest(filename: String): Ast = parseTest(parseFile(filename))

  // tests for ES interpreter
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

  // tests for ES analyzer
  def checkExit(absSem: AbsSemantics): AbsSemantics =
    assert(absSem.finalResult.value.getSingle == FlatElem(ASimple(Undef)))
    absSem
  def analyzeTest(str: String): AbsSemantics = checkExit(analyze(str))
  def analyzeTestFile(filename: String): AbsSemantics =
    checkExit(analyzeFile(filename))
}
