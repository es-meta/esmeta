package esmeta.spec

/** ECMAScript specifications (ECMA-262) */
case class Spec(
  version: Option[String],
  grammar: Grammar,
  algorithms: List[Algorithm],
) extends SpecElem
