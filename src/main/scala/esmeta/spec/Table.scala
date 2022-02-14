package esmeta.spec

// -----------------------------------------------------------------------------
// tables
// -----------------------------------------------------------------------------
case class Table(
  id: String,
  header: List[String],
  rows: List[List[String]],
) extends SpecElem
