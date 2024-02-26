package esmeta.es.util

import esmeta.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.error.*
import sys.process.*
import java.util.StringJoiner
import scala.util.Try

/** ECMAScript program validity checker */
object ValidityChecker {
  def apply(grammar: Grammar, ast: Ast): Boolean =
    apply(ast.toString(grammar = Some(grammar)))
  def apply(code: String): Boolean =
    val src = s"${USE_STRICT}throw \"$MESSAGE\";$LINE_SEP;$LINE_SEP$code"
    (!runUsingSh(";").isSuccess || _valid(runUsingSh(src)))

  val MESSAGE = "VALIDITY_CHECKER_EXPECTED_EXCEPTION"

  private def _valid(result: Try[Any]): Boolean =
    result.failed.filter(_.getMessage contains MESSAGE).isSuccess

  def runUsingSh(src: String): Try[String] =
    Try {
      val escapedSrc = escape(src)
      val stdout = new StringJoiner(LINE_SEP)
      val stderr = new StringJoiner(LINE_SEP)
      val pb: ProcessBuilder =
        s"timeout -k 10s 3s node --unhandled-rejections=none -e $escapedSrc"

      pb ! ProcessLogger(
        out => stdout.add(out),
        err => stderr.add(err),
      ) match {
        case 0         => stdout.toString
        case 124 | 137 => throw TimeoutException("")
        case 127       => throw NoCommandError("")
        case st        => throw new Exception(stdout.toString + stderr.toString)
      }
    }

  /** escape a string to a shell-safe string, enclosed by single quote */
  private def escape(string: String): String =
    val replaced = string.replace("'", "'\"'\"'") // replace I'm to I'"'"'m
    s"'$replaced'"
}
