package esmeta.spec.util

import org.jsoup.nodes.Element

/** extensions for Elements */
extension (elem: Element) {

  /** walker for ancestors */
  def walkAncestor[T](
    f: Element => T,
    base: T,
    join: (T, T) => T,
  ): T =
    val parent = elem.parent
    if (parent == null) base
    else join(f(parent), parent.walkAncestor(f, base, join))

  /** checks whether an element is in appendix */
  def isInAnnex: Boolean =
    elem.walkAncestor(_.tagName == "emu-annex", false, _ || _)

  /** checks whether an element is in regeular expression section */
  def isInRegexp: Boolean =
    elem.walkAncestor(
      _.id == "sec-regexp-regular-expression-objects",
      false,
      _ || _,
    )
}
