package esmeta.ir

import esmeta.ir.util.*

// TODO ir types
case class Type(name: String) extends IRElem
object Type extends Parser.From[Type]
