package esmeta.lang

import esmeta.lang.util.*

// metalanguage types
// TODO more detailed instead of strings
case class Type(name: String) extends Syntax
object Type extends Parser.From[Type]
