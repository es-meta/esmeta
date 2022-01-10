package esmeta.spec

import org.jsoup.nodes.*

object Utils {
  import Symbol.*

  /** get sorted productions */
  def sort(prods: List[Production]): List[Production] =
    prods.sortBy(prod => (prod.kind.ordinal, prod.lhs.name))

  /** walker for ancestors */
  def walkAncestor[T](
    elem: Element,
    f: Element => T,
    base: T,
    join: (T, T) => T,
  ): T =
    val parent = elem.parent
    if (parent == null) base
    else join(f(parent), walkAncestor(parent, f, base, join))

  /** checks whether an element is in appendix */
  def isInAnnex(elem: Element): Boolean =
    walkAncestor(elem, _.tagName == "emu-annex", false, _ || _)

  /** get the index mapping for grammars */
  def getIdxMap(
    grammar: Grammar,
    forWeb: Boolean = false,
  ): Map[String, (Int, Int)] = (for {
    prod <- if (forWeb) grammar.prodsForWeb else grammar.prods
    pair <- getIdxMap(prod)
  } yield pair).toMap

  /** get the index mapping for productions */
  def getIdxMap(prod: Production): Map[String, (Int, Int)] = (for {
    (rhs, i) <- prod.rhsList.zipWithIndex
    (name, j) <- allNames(rhs).zipWithIndex
  } yield prod.lhs.name + ":" + name -> (i, j)).toMap

  /** get rhs all names */
  def allNames(rhs: Rhs): List[String] =
    rhs.symbols.foldLeft(List[String]("")) {
      case (names, Terminal(term)) => names.map(_ + term)
      case (names, Nonterminal(name, _, optional)) =>
        names.flatMap(x => {
          if (optional) List(x, x + name) else List(x + name)
        })
      case (names, ButNot(Nonterminal(base, _, _), cases)) =>
        names.map(_ + base)
      case (names, _) => names
    }

  /** get non-terminals in an RHS */
  def getNTs(rhs: Rhs): List[Nonterminal] = rhs.symbols.flatMap(getNT)

  /** get an non-terminal or nothing from a symbol */
  def getNT(symbol: Symbol): Option[Nonterminal] = symbol match {
    case (nt: Nonterminal) => Some(nt)
    case ButNot(base, _)   => Some(base)
    case _                 => None
  }
}
