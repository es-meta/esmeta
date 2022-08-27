package esmeta.typing.util

import esmeta.typing.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** metalanguage parser */
object Parser extends Parsers
trait Parsers extends BasicParsers {
  given ty: Parser[Ty] = {
    "" ^^ { _ => ??? }
  }.named("typing.Ty")
}
