package esmeta.es

import esmeta.ESMetaTest
import esmeta.analyzer.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.es.util.*
import esmeta.interpreter.*
import esmeta.ir.NormalInst
import esmeta.parser.AstFrom
import esmeta.spec.Spec
import esmeta.state.*
import esmeta.util.*
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

  // file extension converter from .mjs to .ir
  lazy val mjs2ir = changeExt("mjs", "ir")

  // check whether a file is a module imported by other test files
  def isFixture(filename: String): Boolean =
    removedExt(filename).endsWith("_FIXTURE")

  // ---------------------------------------------------------------------------
  // parser helpers
  // ---------------------------------------------------------------------------
  // parse ES codes
  lazy val scriptParser: AstFrom = spec.scriptParser
  def parse(str: String): Ast = scriptParser.from(str)
  def parseFile(filename: String): Ast = scriptParser.fromFile(filename)

  // parse ES modules
  lazy val moduleParser: AstFrom = spec.moduleParser
  def parseModule(str: String): Ast = moduleParser.from(str)
  def parseModuleFile(filename: String): Ast = moduleParser.fromFile(filename)

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
    str: String,
    checkAfter: List[NormalInst] = Nil,
    filename: Option[String] = None,
  ): State =
    val (ast, code) = cfg.scriptParser.fromWithCode(str)
    new CheckAfter(cfg.init.from(code, ast, filename), checkAfter).result
  def evalFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = eval(readFile(filename), checkAfter, Some(filename))

  // eval ES modules
  def evalModuleFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
  ): State =
    new CheckAfter(cfg.init.fromModuleFile(filename), checkAfter).result

  // ---------------------------------------------------------------------------
  // analyzer helpers
  // ---------------------------------------------------------------------------
  // TODO ES code analyzer
  // lazy val analyzer = ESAnalyzer(cfg)
  // import analyzer.*

  // TODO analyze ES codes
  // def analyzeFile(filename: String): analyzer.Semantics =
  //   analyzer(readFile(filename).trim)
  // def analyze(str: String): Semantics = analyzer(str)

  // tests for ES parser
  def parseTest(ast: Ast): Ast =
    val newAst = parse(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast = parseTest(parse(str))
  def parseFileTest(filename: String): Ast = parseTest(parseFile(filename))
  def parseModuleTest(ast: Ast): Ast =
    val newAst = parseModule(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseModuleFileTest(filename: String): Ast =
    parseModuleTest(parseModuleFile(filename))

  // tests for ES interpreter
  def checkExit(st: State): st.type = st(GLOBAL_RESULT) match
    case Undef => st
    case v     => fail(s"return not undefined: $v")
  def evalTest(
    str: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = checkExit(eval(str, checkAfter))
  def evalTestFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = checkExit(evalFile(filename, checkAfter))

  // TODO tests for ES analyzer
  // def checkExit(absSem: AbsSemantics): AbsSemantics =
  //   assert(absSem.finalResult.value.getSingle == One(Undef))
  //   absSem
  // def analyzeTest(str: String): AbsSemantics = checkExit(analyze(str))
  // def analyzeTestFile(filename: String): AbsSemantics =
  //   checkExit(analyzeFile(filename))
}
