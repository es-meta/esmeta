package esmeta.lang

import esmeta.util.IndentParsers
import scala.util.matching.Regex

import Step.*, Block.*

/** language parsers */
trait Parsers extends IndentParsers {
  // TODO algorithm blocks
  given block: Parser[Block] = indent ~> (
    rep1(next ~ "1." ~> step) ^^ { Order(_) } |
      rep1(next ~ "*" ~> step) ^^ { Unorder(_) } |
      next ~> figureStr ^^ { Figure(_) }
  ) <~ dedent

  // TODO algorithm steps
  given step: Parser[Step] = yet

  lazy val figureStr: Parser[String] = "<figure>\n".r ~> repsep(
    ".*".r.filter(_.trim != "</figure>"),
    "\n",
  ) <~ "\n *</figure>".r ^^ { _.mkString("\n") }

  lazy val yet: Parser[Yet] =
    ".+".r ~ opt(block) ^^ { case s ~ b => Yet(s, b) }
}
