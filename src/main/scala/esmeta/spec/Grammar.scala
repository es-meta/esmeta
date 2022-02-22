package esmeta.spec

import esmeta.spec.util.*

/** ECMAScript grammars */
case class Grammar(
  prods: List[Production] = Nil,
  prodsForWeb: List[Production] = Nil,
) extends SpecElem {

  /** a mappging from names to productions */
  lazy val nameMap: Map[String, Production] =
    (for (prod <- prods) yield prod.lhs.name -> prod).toMap

  /** get the lexical production names reachable by syntactic productions */
  lazy val topLevelLexicals: Set[String] = (for {
    prod <- prods
    if prod.kind == Production.Kind.Syntactic
    nt <- prod.nts
    name = nt.name
    prodInRhs = nameMap(name)
    if prodInRhs.kind == Production.Kind.Lexical
  } yield name).toSet

  /** the index mapping for grammars */
  lazy val idxMap: Map[String, (Int, Int)] = getIdxMap(false)

  /** get the index mapping for grammars (or extended one for web) */
  def getIdxMap(forWeb: Boolean): Map[String, (Int, Int)] = (for {
    prod <- if (forWeb) prodsForWeb else prods
    pair <- prod.idxMap
  } yield pair).toMap
}
