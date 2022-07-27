package esmeta.analyzer.domain

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.ir.*
import esmeta.js
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.StateMonad
import scala.annotation.targetName // TODO remove this

/** abstract states for type analysis */
object TypeStateDomain extends StateDomain {

  // appender
  given rule: Rule[Elem] = (app, elem) => {
    val irStringifier = IRElem.getStringifier(true, false)
    import irStringifier.given
    if (elem.isBottom) app >> "⊥"
    else
      app.wrap {
        app :> "locals: " >> elem.locals >> LINE_SEP
        app :> "globals: " >> elem.globals >> LINE_SEP
      }
  }

  // constructors
  def apply(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    globals: Map[Global, AbsValue],
  ): Elem = Elem(reachable, locals, globals)

  // elements
  case class Elem(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    globals: Map[Global, AbsValue],
  ) extends StateElemTrait {

    // getters
    def apply(base: AbsValue, prop: AbsValue): AbsValue = ???
    def apply(loc: Loc): AbsObj = ???

    // define global variables
    def defineGlobal(pairs: (Global, AbsValue)*): Elem =
      bottomCheck(pairs.unzip._2) { copy(globals = globals ++ pairs) }

    // define local variables
    def defineLocal(pairs: (Local, AbsValue)*): Elem =
      bottomCheck(pairs.unzip._2) { copy(locals = locals ++ pairs) }

    // setters
    def update(x: Id, value: AbsValue): Elem = ???
    def update(aloc: AbsValue, prop: AbsValue, value: AbsValue): Elem = ???

    // object operators
    def delete(refV: AbsRefValue): Elem = ???
    def append(loc: AbsLoc, value: AbsValue): Elem = ???
    def prepend(loc: AbsLoc, value: AbsValue): Elem = ???
    def remove(loc: AbsLoc, value: AbsValue): Elem = ???
    def pop(loc: AbsLoc, front: Boolean): (AbsValue, Elem) = ???
    def copyObj(from: AbsLoc)(to: AllocSite): Elem = ???
    def keys(loc: AbsLoc, intSorted: Boolean)(to: AllocSite): Elem = ???
    def allocMap(tname: String, pairs: List[(AbsValue, AbsValue)])(
      to: AllocSite,
    ): Elem = ???
    def allocList(list: List[AbsValue])(to: AllocSite): Elem = ???
    def allocSymbol(desc: AbsValue)(to: AllocSite): (AbsValue, Elem) = ???
    def setType(loc: AbsLoc, tname: String): Elem = ???
    def contains(loc: AbsLoc, value: AbsValue): AbsBool = ???

    // singleton location checks
    def isSingle(loc: Loc): Boolean = ???

    // find merged parts
    def findMerged: Unit = ???

    // handle calls
    def doCall: Elem = ???
    def doProcStart(fixed: Set[Loc]): Elem = ???

    // handle returns (this: return states / to: caller states)
    def doReturn(to: Elem, defs: (Local, AbsValue)*): Elem = ???
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = ???
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem = ???
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = ???
    def garbageCollected: Elem = ???

    // get reachable locations
    def reachableLocs: Set[Loc] = ???

    // copy
    def copied(
      locals: Map[Local, AbsValue] = Map(),
    ): Elem = copy(locals = locals)

    // conversion to string
    def toString(detail: Boolean = false): String = {
      val app = new Appender
      app >> this
      app.toString
    }

    // get string wth detailed shapes of locations
    def getString(value: AbsValue): String = ???
  }
}
