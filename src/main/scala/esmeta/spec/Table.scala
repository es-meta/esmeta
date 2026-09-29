package esmeta.spec

/** tables in ECMA-262 */
case class Table(
  id: String,
  caption: String,
  header: List[String],
  rows: List[List[String]],
) extends SpecElem
