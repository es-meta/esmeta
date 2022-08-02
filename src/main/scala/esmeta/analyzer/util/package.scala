package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.Func
import esmeta.error.*
import esmeta.interp.*
import esmeta.ir.{Name, Func => IRFunc, Local}
import esmeta.util.BaseUtils.*
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
// TODO
def warning(
  cp: ControlPoint,
  msg: String,
): Unit = printlnColor(RED)(s"[$cp @ ${cp.func.name}]: $msg")
