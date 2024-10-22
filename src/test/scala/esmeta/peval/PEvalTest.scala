package esmeta.peval

import esmeta.ESMetaTest
import esmeta.cfg.*
import esmeta.cfgBuilder.*
import esmeta.interpreter.*
import esmeta.ir.*
import esmeta.state.*
import org.scalatest.Assertions.*

import esmeta.ESMetaTest
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.es.*
import esmeta.es.util.*
import esmeta.interpreter.*
import esmeta.ir.NormalInst
import esmeta.parser.AstFrom
import esmeta.spec.Spec
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import org.scalatest.Assertions.*

trait PEvalTest extends ESMetaTest {
  def category: String = "peval"
}
object PEvalTest {

// file extension converter from .js to .ir
  lazy val js2ir = changeExt("js", "ir")

// ---------------------------------------------------------------------------
// parser helpers
// ---------------------------------------------------------------------------
// parse ES codes
  lazy val scriptParser: AstFrom = ESMetaTest.spec.scriptParser
  def parse(str: String): Ast = scriptParser.from(str)
  def parseFile(filename: String): Ast = scriptParser.fromFile(filename)

// ---------------------------------------------------------------------------
// interpreter helpers
// ---------------------------------------------------------------------------
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
    cfg: CFG,
    str: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
    filename: Option[String] = None,
  ): State =
    new CheckAfter(
      Initialize(cfg, str, cachedAst, filename),
      checkAfter,
    ).result
  def evalFile(
    cfg: CFG,
    filename: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State =
    eval(cfg, readFile(filename), checkAfter, cachedAst, Some(filename))

// ---------------------------------------------------------------------------
// analyzer helpers
// ---------------------------------------------------------------------------

// tests for ES parser
  def parseTest(ast: Ast): Ast =
    val newAst = parse(ast.toString(grammar = Some(ESMetaTest.grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast = parseTest(parse(str))
  def parseFileTest(filename: String): Ast = parseTest(parseFile(filename))

// tests for ES interpreter
  def checkExit(st: State): st.type = st(GLOBAL_RESULT) match
    case Undef => st
    case v     => fail(s"return not undefined: $v")
  def evalTest(
    cfg: CFG,
    str: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State = checkExit(eval(cfg, str, checkAfter, cachedAst))
  def evalTestFile(
    cfg: CFG,
    filename: String,
    checkAfter: List[NormalInst] = Nil,
    cachedAst: Option[Ast] = None,
  ): State = checkExit(evalFile(cfg, filename, checkAfter, cachedAst))
}
