package esmeta.analyzer

import esmeta.util.Appender.*

trait ViewLikeDecl { self: Analyzer =>

  /** view abstraction for analysis sensitivities */
  trait ViewLike {

    /** empty check */
    def isEmpty: Boolean
  }

  /** appender */
  def viewRule(detail: Boolean): Rule[View]

  /** empty view */
  val emptyView: View

  /** get entry views of loops */
  def getEntryView(view: View): View

  given viewOrdering: Ordering[View]
}
