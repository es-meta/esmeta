package esmeta.peval.pstate

import esmeta.peval.*
import esmeta.state.*
import esmeta.ir.{Func, Local}
import scala.collection.mutable.{Map => MMap}

case class PContext(func: Func, locals: MMap[Local, Predict[Value]])
