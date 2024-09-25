package esmeta.peval

import esmeta.peval.*
import esmeta.state.*
import esmeta.ir.{Expr, Func, Local}
import scala.collection.mutable.{Map as MMap}

case class PContext(
  func: Func,
  locals: MMap[Local, Predict[Value]],
  sensitivity: Int, // use callCount
  var ret: Option[Predict[Value]],
  var pathCondition: List[Expr],
) {
  self =>

  def copied: PContext = PContext(
    func,
    locals.clone(),
    sensitivity,
    ret,
    pathCondition,
  )

}
