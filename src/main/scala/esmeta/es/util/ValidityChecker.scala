package esmeta.es.util

import scala.util.Try
import esmeta.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.es.util.Engine
import esmeta.util.BaseUtils.warn

/** ECMAScript program validity checker */
object ValidityChecker {

  /** Default timeout for validity check */
  val DEFAULT_TIMEOUT = 1_000 // 1 second

  /** MESSAGE used for validity check */
  val MESSAGE = "VALIDITY_CHECKER_EXPECTED_EXCEPTION"

  /** Check the validity of ECMAScript AST using engines */
  def apply(grammar: Grammar, ast: Ast): Boolean =
    apply(ast.toString(grammar = Some(grammar)))

  /** Check the validity of ECMAScript code using engines */
  def apply(code: String): Boolean =
    val src = s"${USE_STRICT}throw \"$MESSAGE\";$LINE_SEP;$LINE_SEP$code"
    Engine.engines.find(_.canUse) match
      case Some(engine) => checkValid(engine.run(src, Some(DEFAULT_TIMEOUT)))
      case None =>
        warn("No Engine available. This may pass invalid program.")
        true

  /** Check the validity of code using engines */
  private def checkValid(result: Try[Any]): Boolean =
    result.failed.filter(_.getMessage.contains(MESSAGE)).isSuccess
}
