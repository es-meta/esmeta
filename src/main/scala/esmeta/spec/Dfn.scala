package esmeta.spec

/** glossaries defined by `dfn` elements */
case class Dfn(
  name: String,
  variants: List[String] = Nil,
) extends SpecElem {

  /** all forms of this glossary term (its name and variants) */
  def forms: List[String] = name :: variants
}
