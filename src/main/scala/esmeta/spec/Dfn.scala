package esmeta.spec

/** glossary definition defined by `dfn` elements */
case class Dfn(
  name: String,
  variants: List[String] = Nil,
) extends SpecElem {

  /** all forms of this glossary term (its name and variants) */
  def forms: List[String] = name :: variants
}
