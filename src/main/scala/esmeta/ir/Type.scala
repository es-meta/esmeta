package esmeta.ir

import esmeta.ir.util.Parser

// TODO ir types
case class Type(name: String) extends IRElem {
  // TODO more precisely represent type
  def isCompletion: Boolean = name contains "Completion"

}
object Type extends Parser.From[Type]
val UnknownType = Type("unknown")
