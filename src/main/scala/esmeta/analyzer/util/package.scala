package esmeta.analyzer.util

import esmeta.ANALYZE_LOG_DIR
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.error.*
import esmeta.interp.*
import esmeta.ir.{Name, Func => IRFunc, Local}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.Console.RED
import scala.annotation.tailrec

/** get local variables */
import IRFunc.Param
def getLocals(
  func: Func,
  args: List[AbsValue],
  cont: Boolean = false,
): Map[Local, AbsValue] = {
  val params = func.irFunc.params
  var map = Map[Local, AbsValue]()

  @tailrec
  def aux(ps: List[Param], as: List[AbsValue]): Unit = (ps, as) match {
    case (Nil, Nil) =>
    case (Param(lhs, optional, _) :: pl, Nil) =>
      if (optional) {
        map += lhs -> AbsValue(Absent)
        aux(pl, Nil)
      } else throw AnalysisRemainingParams(ps)
    case (Nil, args) =>
      // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
      if (!cont) throw AnalysisRemainingArgs(args)
    case (param :: pl, arg :: al) =>
      map += param.lhs -> arg
      aux(pl, al)
  }
  aux(params, args)
  map
}

/** alarm */
// TODO refactor
var CURRENT_CP: Option[ControlPoint] = None
val nfWarning = getPrintWriter(s"$ANALYZE_LOG_DIR/warnings")
def warning(msg: String): Unit = warning(msg, CURRENT_CP.get)
def warning(msg: String, cp: ControlPoint): Unit =
  val str = s"[$cp @ ${cp.func.name}]: $msg"
  printlnColor(RED)(str)
  nfWarning.println(str)
  nfWarning.flush
// val nfAlarm = getPrintWriter(s"$ANALYZE_LOG_DIR/alarms")
// def alarm(msg: String): Unit =
//   printlnColor(RED)(msg)
//   nfAlarm.println(msg)
//   nfAlarm.flush

/** for debugging */
var REPL_STOP = false

/** exploded */
def exploded(msg: String): Nothing = throw AnalysisImprecise(msg)
