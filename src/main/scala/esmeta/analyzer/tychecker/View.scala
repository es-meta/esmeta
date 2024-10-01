package esmeta.analyzer.tychecker

import esmeta.util.Appender.*

trait ViewDecl { self: TyChecker =>

  /** view abstraction for analysis sensitivities */
  object View extends ViewLike {

    /** empty check */
    def isEmpty: Boolean = true

    /** conversion to string */
    override def toString: String = ""
  }
  type View = View.type

  /** empty view */
  val emptyView: View = View

  /** get entry views of loops */
  def getEntryView(view: View): View = view
}
