package esmeta.analyzer.es

import esmeta.cfg.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

/** view abstraction */
trait ViewDecl { self: ESAnalyzer =>

  /** view abstraction for analysis sensitivities */
  case class View(
    // calling contexts
    calls: List[Call] = Nil,
    // loop contexts
    loops: List[LoopCtxt] = Nil,
    // intra-procedural loop depth
    loopDepth: Int = 0,
  ) extends ViewLike {

    /** empty check */
    def isEmpty: Boolean = this == View()
  }

  /** appender */
  given viewRule: Rule[View] = (app, view) => {
    def auxRule[T](
      name: String,
    )(using Rule[T]): Rule[Iterable[T]] = (app, iter) =>
      given Rule[Iterable[T]] = iterableRule(s"[$name: ", ", ", "]")
      if (iter.isEmpty) app else app >> iter
    given cRule: Rule[Call] = (app, call) => app >> call.id
    given csRule: Rule[Iterable[Call]] = auxRule("call")
    given lRule: Rule[LoopCtxt] = {
      case (app, LoopCtxt(loop, depth)) => app >> s"${loop.id}($depth)"
    }
    given lsRule: Rule[Iterable[LoopCtxt]] = auxRule("loop")
    app >> view.calls >> view.loops
  }

  /** empty view */
  val emptyView: View = View()

  /** get entry views of loops */
  @tailrec
  final def getEntryView(view: View): View =
    if (view.loopDepth == 0) view
    else getEntryView(loopExit(view))

  /** loop context */
  case class LoopCtxt(loop: Branch, depth: Int)

  /** loop transition for next views */
  def loopNext(view: View): View = view.loops match {
    case LoopCtxt(loop, k) :: rest =>
      view.copy(loops = LoopCtxt(loop, k + 1) :: rest)
    case _ => view
  }

  /** loop transition for function enter */
  def loopEnter(view: View, loop: Branch): View = {
    val loopView = view.copy(
      loops = LoopCtxt(loop, 0) :: view.loops,
      loopDepth = view.loopDepth + 1,
    )
    loopOut += loopView -> (loopOut.getOrElse(loopView, Set()) + view)
    loopView
  }

  /** loop transition for bases */
  def loopBase(view: View): View = view.loops match {
    case LoopCtxt(loop, k) :: rest =>
      view.copy(loops = LoopCtxt(loop, 0) :: rest)
    case _ => view
  }

  /** loop transition for function exits */
  def loopExit(view: View): View = {
    val views = loopOut.getOrElse(loopBase(view), Set())
    views.size match
      case 0 => error("invalid loop exit")
      case 1 => views.head
      case _ => exploded("loop is too merged.")
  }
}
