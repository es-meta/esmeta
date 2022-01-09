package esmeta.spec

import scala.util.parsing.combinator.*

/** base parsers */
trait BaseParsers extends RegexParsers {
  protected override val whiteSpace = "[ \t]*//.*|[ \t]+".r
  lazy val newline = "\r?\n|\r|\f".r
  lazy val word = "\\w+".r
}
