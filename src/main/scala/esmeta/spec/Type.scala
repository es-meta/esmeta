package esmeta.spec

import esmeta.spec.util.Parser

// metalanguage types
// TODO more precisely represent metalanguage types
case class Type(name: String = "unknown") extends SpecElem
object Type extends Parser.From[Type]
val UnknownType: Type = Type("unknown")
