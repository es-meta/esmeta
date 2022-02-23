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

  /** checks whether an element is of Chapter 5. Notational Conventions */
  def isNotation: Boolean =
    elem.parent match {
      case null => false
      case parent =>
        if (parent.id == "sec-notational-conventions") true
        else parent.isNotation
    }
}
