package esmeta.analyzer.tychecker

import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.Appender.*

trait ViewDecl { self: TyChecker =>

  /** view abstraction for analysis sensitivities */
  case class View(tys: List[ValueTy] = Nil) extends ViewLike {

    /** empty check */
    def isEmpty: Boolean = tys.isEmpty

    /** conversion to string */
    override def toString: String = ""
  }

  /** appender */
  given viewRule: Rule[View] = (app, view) => {
    import TyStringifier.given
    given Rule[List[ValueTy]] = iterableRule[ValueTy]("[", ", ", "]")
    app >> view.tys
  }

  /** empty view */
  val emptyView: View = View()

  /** get entry views of loops */
  def getEntryView(view: View): View = view
}
