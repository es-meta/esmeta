package esmeta.spec

import org.jsoup.nodes.Document

/** ECMAScript specifications (ECMA-262) */
case class Spec(
  version: Option[String],
  grammar: Grammar,
  algorithms: List[Algorithm],
  document: Document,
) extends SpecElem
