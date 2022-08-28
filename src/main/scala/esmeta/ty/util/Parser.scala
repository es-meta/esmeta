package esmeta.ty.util

import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** metalanguage parser */
object Parser extends Parsers
trait Parsers extends BasicParsers {
  given ty: Parser[Ty] = {
    "" ^^ { _ => ??? }
  }.named("ty.Ty")
}
