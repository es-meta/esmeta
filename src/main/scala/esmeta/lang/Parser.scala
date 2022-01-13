package esmeta.lang

import esmeta.util.BasicParser

/** language parser */
trait Parser[T] extends BasicParser[T, Parsers] { val parser = Parser }
object Parser extends Parsers
