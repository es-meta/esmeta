package esmeta.js.util

import esmeta.js.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.util.parsing.input.Position
import scala.util.matching.Regex

/** JavaScript parser */
case class Parser(val grammar: Grammar) extends LAParsers {
  def lex(name: String, k: Int = 0): String => String =
    str => parseAll(lexers((name, k)), str).get
}
