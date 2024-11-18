package esmeta.peval.pstate

import esmeta.peval.*
import esmeta.state.*
import esmeta.ir.{Expr, Func, Local}
import esmeta.util.{Flat}
import scala.collection.mutable.{Map as MMap}

case class PContext(
  func: Func,
  sensitivity: Int, // use callCount
  locals: MMap[Local, Predict[Value]],
  var ret: Flat[Value],
  // var pathCondition: List[Expr],
) {
  self =>

  def copied: PContext = PContext(
    func,
    sensitivity,
    locals.clone(),
    ret,
  )

}
