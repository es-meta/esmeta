package esmeta.spec

import esmeta.spec.util.*
import esmeta.es.*
import esmeta.util.BaseUtils.cached

/** ECMAScript grammars */
case class Grammar(
  prods: List[Production] = Nil,
  prodsForWeb: List[Production] = Nil,
) extends SpecElem {

  /** a mapping from names to productions */
  lazy val nameMap: Map[String, Production] =
    (for (prod <- prods) yield prod.lhs.name -> prod).toMap

  /** get the lexical production names reachable by syntactic productions */
  lazy val topLevelLexicals: Set[String] = (for {
    prod <- prods
    if prod.kind == ProductionKind.Syntactic
    nt <- prod.nts
    name = nt.name
    prodInRhs = nameMap(name)
    if prodInRhs.kind == ProductionKind.Lexical
  } yield name).toSet

  /** get lexical names */
  lazy val lexicalNames: Set[String] = (for {
    prod <- prods if prod.kind != ProductionKind.Syntactic
  } yield prod.name).toSet

  /** the index mapping for grammars */
  lazy val idxMap: Map[String, (Int, Int)] = getIdxMap(false)

  /** get the index mapping for grammars (or extended one for web) */
  def getIdxMap(forWeb: Boolean): Map[String, (Int, Int)] = (for {
    prod <- if (forWeb) prodsForWeb else prods
    pair <- prod.idxMap
  } yield pair).toMap
}
object Grammar extends Parser.From(Parser.grammarParser)
