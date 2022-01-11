package esmeta.lang

import esmeta.util.BasicParser

/** language parser */
trait Parser[T] extends BasicParser[T] with Parsers
object Parser extends Parsers
