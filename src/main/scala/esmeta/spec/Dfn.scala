package esmeta.spec

/** glossary definition defined by `dfn` elements in ECMA-262 */
case class Dfn(
  name: String, // text of the `dfn` element (e.g., `Completion Record`)
  variants: List[String] =
    Nil, // `variants` attribute (e.g., `Completion Records`)
  id: Option[String] = None, // `id` attribute if exists
  clauseId: String = "", // id of the enclosing `emu-clause` element
) extends SpecElem {

  /** all forms of this glossary term (its name and variants) */
  def forms: List[String] = name :: variants
}
