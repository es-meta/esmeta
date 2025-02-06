package esmeta.analyzer.es

import esmeta.util.Appender.*

/** view abstraction */
trait ViewDecl { self: ESAnalyzer =>

  /** view abstraction for analysis sensitivities */
  case class View() extends ViewLike {

    /** empty check */
    def isEmpty: Boolean = ???
  }

  /** appender */
  def viewRule(detail: Boolean): Rule[View] = ???

  /** empty view */
  val emptyView: View = ???

  /** get entry views of loops */
  def getEntryView(view: View): View = ???
}
