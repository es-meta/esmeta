package esmeta.analyzer.eoggen

import esmeta.cfg.*
import esmeta.util.Appender.*

/** view abstraction */
trait ViewDecl { self: EOGGenerator =>
  import irStringifier.given

  /** view abstraction for analysis sensitivities */
  case class View(calls: List[Call] = Nil) extends ViewLike {

    /** empty check */
    def isEmpty: Boolean = calls.isEmpty

    /** add a call */
    def +(call: Call): View = View(call :: calls)
  }

  /** appender */
  def viewRule(detail: Boolean): Rule[View] = (app, view) => {
    given Rule[Call] = (app, call) => app >> call.id
    given Rule[List[Call]] = iterableRule("[", ", ", "]")
    app >> view.calls
  }

  /** empty view */
  val emptyView: View = View()

  /** get entry views of loops */
  def getEntryView(view: View): View = view
}
