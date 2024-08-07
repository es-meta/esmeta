package esmeta.peval

import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.*
import esmeta.es.builtin.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.state.{Addr, Obj, StateElem}
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}

/** IR Heap for partial Evaluation. similar to state/Heap.scala */
case class PHeap[T](
  val map: MMap[Addr, Obj] = MMap(),
  var size: Int = 0,
) extends StateElem {}
