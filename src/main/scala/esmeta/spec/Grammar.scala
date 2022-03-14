package esmeta.spec

import esmeta.spec.util.*
import esmeta.js.*
import esmeta.util.BaseUtils.cached

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

  /** get sub index of parsed Ast */
  val getSubIdx = cached[Ast, Int] {
    case lex: Lexical => 0
    case Syntactic(name, _, rhsIdx, children) =>
      val rhs = nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        (opt, child) <- rhs.nts.map(_.optional) zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
  }
}
