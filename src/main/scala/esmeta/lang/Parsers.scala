package esmeta.lang

import esmeta.util.BasicParsers
import scala.util.matching.Regex

/** language parsers */
trait Parsers extends BasicParsers {
  // skip only white spaces and comments
  protected override val whiteSpace = "[ \t]*//.*|[ \t]+".r

  private lazy val newlineWithIndent: Parser[Int] = "\n *".r ^^ { _.length - 1 }

  given literal: Conversion[String, Parser[String]] = s => {
    val slist = s.split(' ').toList
    (slist.map(super.literal).reduce(_ ~ _ ^^ { case x ~ y => x + " " + y }))
  }

  // TODO algorithm steps
  given step: Parser[Step] = rep(newlineWithIndent | ".+".r) ^^^ Step.Block(Nil)
}
