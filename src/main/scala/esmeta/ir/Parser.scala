package esmeta.ir

import esmeta.util.BasicParser

/** IR parser */
trait Parser[T] extends BasicParser[T, Parsers] { val parser = Parser }
object Parser extends Parsers
