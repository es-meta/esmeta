package esmeta.ty

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.state.*
import scala.collection.mutable.{Map => MMap}

/** contains test */
class ContainsTinyTest extends TyTest {
  val name: String = "tyContainsTest"

  // registration
  def init: Unit = {
    lazy val func = Func(
      0,
      IRFunc(true, IRFuncKind.AbsOp, "f", Nil, UnknownType, INop()),
      Block(0),
    )
    lazy val ctxt = Context(func, MMap())
    // TODO fill out
    lazy val heap = Heap(MMap(DynamicAddr(0) -> MapObj("A", MMap(), 0)), 1)
    lazy val st = State(CFG(List(func)), ctxt, heap = heap)

    check("ty.contains") {
      // TODO fill out remaining cases
      assert(NumberTopT.contains(Number(42), st))
    }
  }
  init
}
