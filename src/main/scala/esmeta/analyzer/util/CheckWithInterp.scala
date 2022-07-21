package esmeta.analyzer.util

import esmeta.TEST_MODE
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.js
import esmeta.util.BaseUtils.*

case class CheckWithInterp(
  val sem: AbsSemantics,
  val interp: Interp,
  val execLevel: Int,
) {
  // run and check the soundness
  def runAndCheck: Unit = {
    // run concrete execution
    interp.step

    // check soundness
    worklist.headOption match {
      case None =>
        // unexpected terminations
        if (sem.finalResult.isBottom)
          fail(s"the worklist is empty")
      case Some(cp) =>
        (st.context.cursor, cp) match {
          case (ExitCursor(f0), rp @ ReturnPoint(f1, _)) if f0 == f1 =>
            checkSt(rp, sem(rp).state, f0)
          case (NodeCursor(n0), np @ NodePoint(func, n1, _)) if n0 == n1 =>
            checkSt(np, sem(np), func)
          case (cursor, cp) =>
            fail("Cursor($cursor) is not same with control point($cp)")
        }
    }
  }

  def worklist = sem.worklist
  def st = interp.st
  def fail(msg: String): Unit = fail(msg, None)
  def fail(msg: String, func: Func): Unit = fail(msg, Some(func))
  def fail(msg: String, funcOpt: Option[Func]): Unit = {
    if (TEST_MODE) error(msg) else warn(msg)
    sem.repl.continue = false
  }

  // check soundness of abstract state
  def checkSt(cp: ControlPoint, absSt: sem.AbsState, f: Func): Unit =
    if (execLevel >= 2) {
      // singletone check
      if (!absSt.isSingle)
        fail("the abstract state is not single", f)
      // soundness check
      if (!check(cp, absSt, st))
        fail("the abstract state is not sound", f)
    } else {} // pass
  def checkTy(lty: String, rty: String): Boolean =
    (lty == rty) || { println(s"different type: $lty != $rty"); false }
  def checkLength[T](l: Vector[T], r: Vector[T]): Boolean = (
    l.length == r.length
  ) || {
    println(s"different length: $l (${l.length}) != $r (${r.length})")
    false
  }
  def check(
    np: ControlPoint,
    absSt: sem.AbsState,
    st: State,
  ): Boolean = {
    val sem.AbsState(reachable, absLocals, absGlobals, absHeap) = absSt
    val State(_, context, _, _, _, globals, heap) = st
    val locals = context.locals
    var visited = Set[Loc]()

    def checkValue(absValue: AbsValue, value: Value): Boolean = {
      absValue.getSingle match {
        case FlatElem(avalue) => checkSingleValue(avalue, value)
        case _                => false
      }
    }
    def checkSingleValue(avalue: AValue, value: Value): Boolean = (value match {
      case _: Comp | _: Const | _: AstValue | _: SimpleValue =>
        avalue == AValue(value)
      case addr: Addr =>
        avalue match {
          case loc: Loc =>
            checkLoc(loc, addr)
          case _ => false
        }
      case _ => true
    }) || { println(s"$avalue != $value"); false }
    def checkLoc(loc: Loc, addr: Addr): Boolean = {
      if (visited contains loc) true
      else if (!absSt.heap.map.contains(loc)) {
        if (loc.isNamed) { visited += loc; true }
        else { println(s"$loc is required but removed."); false }
      } else {
        visited += loc
        val absObj = absSt(loc)
        val obj = st(addr)
        import AbsObj._
        (absObj, obj) match {
          case (Bot | MergedMap(_, _, _) | MergedList(_), _) => false
          case (SymbolElem(adesc), SymbolObj(desc)) => checkValue(adesc, desc)
          case (am @ OrderedMap(aty, _, _), m @ MapObj(ty, _, _)) =>
            checkTy(aty, ty) && {
              am.keys(intSorted = false) match {
                case MergedList(_) => false
                case KeyWiseList(aprops) => {
                  val props = m.keys(intSorted = false)
                  val lengthB = checkLength(aprops, props)
                  val propsB = (aprops zip props).forall {
                    case (aprop, prop) =>
                      (
                        checkValue(aprop, prop) &&
                        checkValue(am(aprop), m(prop))
                      ) || {
                        println((aprops, props))
                        println(s"$loc[$aprop] != $addr[$prop]")
                        false
                      }
                  }
                  lengthB && propsB
                }
                case _ => false
              }
            }
          case (KeyWiseList(avalues), ListObj(values)) => {
            checkLength(avalues, values) && (avalues zip values).forall {
              case (aprop, prop) => checkValue(aprop, prop)
            }
          }
          case (NotSupportedElem(aty, adesc), YetObj(tyname, desc)) =>
            checkTy(aty, tyname) && adesc == desc
          case (absObj, irObj) =>
            println(s"$absObj != $irObj")
            false
        }
      }
    }

    val localCheck = (locals.keySet ++ absLocals.keySet).forall(x =>
      (
        checkValue(absSt(x, np), st(x)) || {
          println(s"local variable $x is not sound."); false
        }
      ),
    )

    val globalCheck = (globals.keySet ++ absGlobals.keySet).forall(x =>
      (
        checkValue(absSt(x, np), st(x)) || {
          println(s"global variable $x is not sound."); false
        }
      ),
    )

    val locsCheck = absSt.reachableLocs.forall(loc => {
      loc.isNamed || visited.contains(loc) || absSt.heap.map.contains(loc) || {
        println(s"$loc is required but removed."); false
      }
    })

    reachable && localCheck && globalCheck && locsCheck
  }
}
object CheckWithInterp {
  def apply(
    sem: AbsSemantics,
    sourceText: String,
    cachedAst: Option[js.Ast],
    execLevel: Int,
  ): CheckWithInterp = {
    val initSt = js.Initialize(sem.cfg, sourceText, cachedAst)
    val interp = new Interp(initSt, Nil)
    CheckWithInterp(sem, interp, execLevel)
  }
}
