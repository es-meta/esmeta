package esmeta.test262

import esmeta.*
import esmeta.error.NotSupported
import esmeta.es.*
import esmeta.es.util.*
import esmeta.parser.ESParser
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.spec.Spec

case class Test262(spec: Spec) {
  // parsing result
  type ParseResult = Either[String, List[Ast]]

  // cache for parsing results for necessary harness files
  lazy val getInclude = cached[String, ParseResult](name =>
    try {
      val filename = s"$TEST262_DIR/harness/$name"
      val script = parseFile(filename)
      Right(script.flattenStmt)
    } catch {
      case NotSupported(msg) => Left(msg)
    },
  )

  // parse ECMAScript file
  lazy val scriptParser = spec.scriptParser
  def parseFile(filename: String): Ast = scriptParser.fromFile(filename)

  // test262 test configuration
  lazy val config = TestFilter.configSummary
  lazy val manualConfig = TestFilter.manualSummary

  // basic statements
  lazy val basicStmts = for {
    x <- getInclude("assert.js")
    y <- getInclude("sta.js")
  } yield x ++ y

  // load test262 file with harnesses
  def loadTestFromFile(filename: String): (String, Ast) = {
    val meta = MetaData(filename)
    loadTest(parseFile(filename), meta.includes)
  }
  def loadTest(ast: Ast, includes: List[String]): (String, Ast) = {
    // load harness
    val includeStmts = includes.foldLeft(basicStmts) {
      case (li, s) =>
        for {
          x <- li
          y <- getInclude(s)
        } yield x ++ y
    } match {
      case Right(l)  => l
      case Left(msg) => throw NotSupported(msg)
    }

    // parse test and merge with harnesses
    val stmts = includeStmts ++ flattenStmt(ast)
    val merged = mergeStmt(stmts)

    // result
    val sourceText = merged.toString(grammar = Some(spec.grammar)).trim
    (sourceText, merged)
  }
}
