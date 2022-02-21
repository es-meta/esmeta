package esmeta.test262

import esmeta.*
import esmeta.error.NotSupported
import esmeta.js.*
import esmeta.js.util.{Parser => JSParser, *}
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

  // parse JavaScript file
  lazy val parser = JSParser(spec.grammar)("Script")
  def parseFile(filename: String): Ast = parser.fromFile(filename)

  // test262 test configuration
  lazy val config = TestFilter.configSummary

  // basic statements
  lazy val basicStmts = for {
    x <- getInclude("assert.js")
    y <- getInclude("sta.js")
  } yield x ++ y
}
