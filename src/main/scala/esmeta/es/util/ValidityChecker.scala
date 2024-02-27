package esmeta.es.util

import esmeta.*
import esmeta.es.*
import esmeta.spec.*
import javax.script._

/** ECMAScript program validity checker */
object ValidityChecker {
  val MESSAGE = "VALIDITY_CHECKER_EXPECTED_EXCEPTION"

  def apply(grammar: Grammar, ast: Ast): Boolean =
    apply(ast.toString(grammar = Some(grammar)))
  def apply(code: String): Boolean =
    useGraalJs(code)

  def useGraalJs(code: String): Boolean = {
    val manager = new ScriptEngineManager
    val engine = manager.getEngineByName("Graal.js")
    if (engine == null) {
      Console.err.println(
        "[WARNING] ValidityChecker always return true because Graal.js is not yet installed in this environment.",
      )
      true
    } else
      try {
        val src = s"${USE_STRICT}throw \"$MESSAGE\";$LINE_SEP;$LINE_SEP$code"
        engine.eval(src)
        false
      } catch {
        case e: ScriptException =>
          val pass = e.toString.contains(MESSAGE)
          pass
      }
  }
}
