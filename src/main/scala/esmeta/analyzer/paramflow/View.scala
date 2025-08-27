package esmeta.analyzer.paramflow

import esmeta.util.Appender.*

/** view abstraction */
trait ViewDecl { self: ParamFlowAnalyzer =>

  /** view abstraction for analysis sensitivities */
  object View extends ViewLike {

    /** empty check */
    def isEmpty: Boolean = true
  }
  type View = View.type

  /** appender */
  def viewRule(detail: Boolean): Rule[View] = (app, view) => app

  /** empty view */
  val emptyView: View = View

  /** get entry views of loops */
  def getEntryView(view: View): View = view
}
