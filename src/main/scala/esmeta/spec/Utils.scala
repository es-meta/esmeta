package esmeta.spec

import org.jsoup.nodes.*

object Utils {

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
}
