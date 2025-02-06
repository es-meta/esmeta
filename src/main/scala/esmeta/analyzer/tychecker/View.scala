package esmeta.analyzer.tychecker

import esmeta.ty.*
import esmeta.util.Appender.*

/** view abstraction */
trait ViewDecl { self: TyChecker =>
  import tyStringifier.given

  /** view abstraction for analysis sensitivities */
  case class View(tys: List[ValueTy] = Nil) extends ViewLike {

    /** empty check */
    def isEmpty: Boolean = tys.isEmpty
  }

  /** appender */
  def viewRule(detail: Boolean): Rule[View] = (app, view) => {
    given Rule[List[ValueTy]] = iterableRule[ValueTy]("[", ", ", "]")
    app >> view.tys
  }

  /** empty view */
  val emptyView: View = View()

  /** get entry views of loops */
  def getEntryView(view: View): View = view
}
