package esmeta.spec

object Utils {

  /** get sorted productions */
  def getSortedProds(grammar: Grammar): List[Production] =
    grammar.prods.sortBy(prod => (prod.kind.ordinal, prod.lhs.name))
}
