package esmeta.es.util.dsl

import scala.util.matching.Regex

object PredicateExpr {

  /** Parse a predicate expression string into a compiled Regex. */
  def parse(expr: String): Regex = {
    val tokens = tokenize(expr.trim)
    val regexStr = tokens.map(tokenToRegex).mkString
    ("^" + regexStr + "$").r
  }

  /** Check if a symbolic path matches a predicate regex. */
  def matches(path: List[String], predRegex: Regex): Boolean = {
    val pathStr = path.mkString(".")
    predRegex.matches(pathStr)
  }

  // ---------------------------------------------------------------------------
  // Tokenizer
  // ---------------------------------------------------------------------------

  private sealed trait Token
  private case object Wildcard extends Token // _
  private case class Field(name: String) extends Token // .[[X]]
  private case object CopyStar extends Token // .copy*
  private case object CopyOnce extends Token // .copy
  private case object LParen extends Token // (
  private case object RParen extends Token // )
  private case object Alt extends Token // |

  private def tokenize(expr: String): List[Token] = {
    val tokens = scala.collection.mutable.ListBuffer[Token]()
    var i = 0
    while (i < expr.length) {
      expr(i) match {
        case ' ' | '\t' | '\n' => i += 1 // skip whitespace
        case '_' =>
          tokens += Wildcard; i += 1
        case '(' =>
          tokens += LParen; i += 1
        case ')' =>
          tokens += RParen; i += 1
        case '|' =>
          tokens += Alt; i += 1
        case '.' =>
          if (expr.startsWith(".[[", i)) {
            // .[[fieldName]]
            val end = expr.indexOf("]]", i + 3)
            if (end < 0)
              throw new RuntimeException(
                s"Unclosed [[ in predicate expression: $expr",
              )
            val name = expr.substring(i + 3, end)
            tokens += Field(name)
            i = end + 2
          } else if (expr.startsWith(".copy*", i)) {
            tokens += CopyStar; i += 6
          } else if (expr.startsWith(".copy", i)) {
            tokens += CopyOnce; i += 5
          } else {
            throw new RuntimeException(
              s"Unexpected '.' at position $i in: $expr",
            )
          }
        case c =>
          throw new RuntimeException(
            s"Unexpected char '$c' at position $i in: $expr",
          )
      }
    }
    tokens.toList
  }

  // ---------------------------------------------------------------------------
  // Token → Regex fragment
  // ---------------------------------------------------------------------------

  private def tokenToRegex(token: Token): String = token match {
    case Wildcard => ".*"
    case Field(n) => Regex.quote(s"[[$n]]")
    case CopyStar => s"(\\.${Regex.quote("copy")})*"
    case CopyOnce => s"\\.${Regex.quote("copy")}"
    case LParen   => "("
    case RParen   => ")"
    case Alt      => "|"
  }
}
