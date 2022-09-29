package esmeta.ty

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, FuncKind => IRFuncKind, *}
import esmeta.state.*
import scala.collection.mutable.{Map => MMap}
import esmeta.es.*

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
      assert(BoolT.contains(Bool(true), st))
      assert(BigIntT.contains(BigInt(42), st))
      assert(StrTopT.contains(Str("test"), st))
      assert(MathTopT.contains(Math(52), st))
      assert(
        AstTopT.contains(
          AstValue(Syntactic("", Nil, 5, List(Some(Lexical("name", "str"))))),
          st,
        ),
      )
      assert(
        AstT("name").contains(
          AstValue(
            Syntactic("name", Nil, 5, List(Some(Lexical("name", "str")))),
          ),
          st,
        ),
      )
      assert(GrammarT(Grammar("", Nil)).contains(Grammar("", Nil), st))
      assert(ConstT("const").contains(Const("const"), st))
      assert(CodeUnitT.contains(CodeUnit(' '), st))
      assert(UndefT.contains(Undef, st))
      assert(NullT.contains(Null, st))
      assert(AbsentT.contains(Absent, st))
      assert(AbruptT.contains(Comp(Const(" "), Math(5), None), st))
      // TODO Test for addr, clo, cont
      // Stringifier 참고해서 넣기.
    }
  }
  init
}
