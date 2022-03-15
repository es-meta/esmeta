package esmeta.spec

import esmeta.spec.util.Parser
import esmeta.lang.{Type => LangType}

// metalanguage types
// TODO more precisely represent metalanguage types
case class Type(name: String = "unknown") extends SpecElem {

  /** convert to metalanguage types */
  def toLang: LangType = LangType(name)

  /** normalized name */
  def normalizedName: String = toLang.normalized.name
}
object Type extends Parser.From[Type]
val UnknownType: Type = Type("unknown")
