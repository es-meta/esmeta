package esmeta.analyzer

import esmeta.ESMetaTest
import esmeta.analyzer.tychecker.TyChecker
import esmeta.cfg.{Call, Node}
import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import scala.collection.mutable.{Map => MMap}

/** type guard test */
class TypeGuardSmallTest extends AnalyzerTest {
  val name: String = "analyzerTypeGuardTest"

  private val checker = TyChecker(
    cfg = ESMetaTest.cfg,
    useEffect = true,
    silent = true,
  )
  import checker.*

  private val r = Name("r")
  private val y = Name("y")
  private val v = Name("v")
  private val x = Name("x")
  private val func = checker.cfg.fnameMap("IsArray")
  private given NodePoint[Node] = NodePoint(func, func.entry, emptyView)

  private val arrayProp = TypeProp((y: Base) -> (ArrayT, Provenance.Bot))
  private val completionGuard = TypeGuard(
    TargetType(NormalT(TrueT)) -> arrayProp,
  )
  private val objectTy = RecordT(
    "",
    Map("f" -> (NumberT || StrT), "g" -> BoolT),
  )
  private val argument = SymTy.SField(SymTy.SVar(x), SymTy.STy(StrT("f")))
  private val call = Call(
    -1,
    ICall(r, EStr("g"), List(ERef(Field(x, EStr("f"))), ERef(x))),
  )

  private def completionState: AbsState = AbsState.Empty.copy(locals =
    Map(
      r -> AbsValue(SymTy.STy(CompT), completionGuard),
      y -> AbsValue(ObjectT),
    ),
  )

  private def argumentState: AbsState = AbsState.Empty.copy(
    locals = Map(x -> AbsValue(objectTy)),
  )

  private def assume(st: AbsState, ref: Ref, ty: ValueTy): AbsState =
    val expr = ETypeCheck(ERef(ref), Type(ty))
    val (value, next) = checker.transfer.transfer(expr)(st)
    checker.transfer.refine(expr, value, TrueT)(using next, summon)(next)

  private def completionHeap(kind: String, array: Boolean): Heap = Heap(
    MMap(
      NamedAddr("r") -> RecordObj(
        "CompletionRecord",
        MMap(
          "Type" -> Enum(kind),
          "Value" -> Bool(true),
          "Target" -> Enum("empty"),
        ),
      ),
      NamedAddr("y") -> RecordObj(if array then "Array" else "Object", MMap()),
    ),
  )

  private def argumentHeap: Heap = Heap(
    MMap(
      NamedAddr("x") -> RecordObj(
        "",
        MMap("f" -> Number(1.0), "g" -> Bool(false)),
      ),
    ),
  )

  /** Check stored values, symbolic references, and applicable guard promises.
    */
  private def checkCovered(
    st: AbsState,
    values: Map[Local, Value],
    heap: Heap,
  ): Unit =
    given AbsState = st
    def read(ref: SymRef): Value = ref match
      case SymTy.SVar(local) => values(local)
      case SymTy.SField(base, field) =>
        val addr = read(base) match
          case addr: Addr => addr
          case value      => fail(s"Expected a record address, got $value")
        val key = field.upper.getSingle match
          case One(value: Value) => value
          case _ => fail(s"Expected a concrete field, got $field")
        heap(addr, key)
      case SymTy.SSym(sym) => fail(s"Unexpected caller symbol: $sym")

    def checkProp(prop: TypeProp): Unit =
      assert(prop.sexpr.isEmpty)
      prop.map.foreach {
        case (local: Local, (ty, _)) =>
          assert(
            ty.contains(values(local), heap),
            s"$local does not satisfy $ty",
          )
        case (sym, _) => fail(s"Unexpected caller symbol: $sym")
      }

    assert(!st.isBottom)
    values.foreach { (local, concrete) =>
      val value = st.get(local)
      assert(!value.isBottom)
      assert(
        value.ty.contains(concrete, heap),
        s"$local is not covered by ${value.ty}",
      )
      value.symty match
        case ref: SymRef => assert(read(ref) == concrete)
        case _           => ()
      value.guard.map.foreach { (target, prop) =>
        if target.ty.contains(concrete, heap) then checkProp(prop)
      }
    }
    checkProp(st.prop)

  // registration
  def init: Unit = {
    check("field lookup (abrupt completion)") {
      val before = completionState
      val (field, _) =
        checker.transfer.transfer(Field(r, EStr("Value")))(before)
      val stored = before.update(v, field)
      val heap = completionHeap("throw", false)
      val values = Map[Local, Value](
        r -> NamedAddr("r"),
        y -> NamedAddr("y"),
        v -> Bool(true),
      )
      checkCovered(stored, values, heap)
      assert(field.guard(TrueT).isTop)
      val after = assume(stored, v, TrueT)
      assert(!(after.get(y).ty(using after) <= ArrayT))
      checkCovered(after, values, heap)
    }

    check("refinement (accumulated conditions)") {
      val heap = completionHeap("normal", true)
      val values = Map[Local, Value](r -> NamedAddr("r"), y -> NamedAddr("y"))
      val conditions =
        List((r: Ref) -> NormalT, Field(r, EStr("Value")) -> TrueT)
      for (order <- List(conditions, conditions.reverse)) {
        val before = completionState
        checkCovered(before, values, heap)
        val first = assume(before, order.head._1, order.head._2)
        assert(!(first.get(y).ty(using first) <= ArrayT))
        checkCovered(first, values, heap)
        val after = assume(first, order(1)._1, order(1)._2)
        assert(after.get(r).ty(using after) <= NormalT(TrueT))
        assert(after.get(y).ty(using after) <= ArrayT)
        checkCovered(after, values, heap)
      }
    }

    check("field lookup (single-field target)") {
      // A true field implies Array; a false field provides no refinement.
      val target = new TargetType(RecordT("", Map("f" -> TrueT)))
      val before = AbsState.Empty.copy(locals =
        Map(
          r -> AbsValue(
            SymTy.STy(RecordT("", Map("f" -> BoolT))),
            TypeGuard(target -> arrayProp),
          ),
          y -> AbsValue(ObjectT),
        ),
      )
      val (field, _) = checker.transfer.transfer(Field(r, EStr("f")))(before)
      val stored = before.update(v, field)
      assert(field.guard(TrueT).localEnv(y)._1 == ArrayT)
      assert(field.guard(FalseT).isTop)
      for (result <- List(true, false)) {
        val heap = Heap(
          MMap(
            NamedAddr("r") -> RecordObj("", MMap("f" -> Bool(result))),
            NamedAddr("y") -> RecordObj(
              if result then "Array" else "Object",
              MMap(),
            ),
          ),
        )
        val values = Map[Local, Value](
          r -> NamedAddr("r"),
          y -> NamedAddr("y"),
          v -> Bool(result),
        )
        checkCovered(stored, values, heap)
        val after = assume(stored, v, BoolT(result))
        assert((after.get(y).ty(using after) <= ArrayT) == result)
        checkCovered(after, values, heap)
      }
    }

    check("guard instantiation") {
      val before = argumentState
      val heap = argumentHeap
      val saved = heap(NamedAddr("x"), Str("f"))
      val result = Bool(NumberT.contains(saved, heap))
      checkCovered(before, Map(x -> NamedAddr("x")), heap)
      val callee = AbsValue(
        SymTy.STy(BoolT),
        TypeGuard(
          TargetType(TrueT) -> TypeProp((0: Sym) -> (NumberT, Provenance.Bot)),
        ),
      )
      val effect = Effect.Empty.fieldUpdate("f", objectTy)(using before)
      val returned = checker.transfer.instantiate(
        call,
        callee,
        Map(0 -> AbsValue(argument)),
        effect,
      )(using before)
      heap.update(NamedAddr("x"), Str("f"), Str("abc"))
      val stored = before.weaken(effect).update(r, returned)
      val values = Map[Local, Value](x -> NamedAddr("x"), r -> result)
      checkCovered(stored, values, heap)
      val after = assume(stored, r, TrueT)
      assert(!(after.get(x).ty(using after).record("f").value <= NumberT))
      checkCovered(after, values, heap)
    }

    check("symbolic instantiation (changed field)") {
      val before = argumentState
      val heap = argumentHeap
      val saved = heap(NamedAddr("x"), Str("f"))
      checkCovered(before, Map(x -> NamedAddr("x")), heap)
      val effect = Effect.Empty.fieldUpdate("f", objectTy)(using before)
      val returned = checker.transfer.instantiate(
        call,
        AbsValue(SymTy.SSym(0)),
        Map(0 -> AbsValue(argument)),
        effect,
      )(using before)
      heap.update(NamedAddr("x"), Str("f"), Str("abc"))
      val after = before.weaken(effect).update(r, returned)
      assert(!returned.symty.isInstanceOf[SymTy.SField])
      assert(saved != heap(NamedAddr("x"), Str("f")))
      checkCovered(after, Map(x -> NamedAddr("x"), r -> saved), heap)
    }

    check("symbolic instantiation (unchanged field)") {
      val before = argumentState
      val heap = argumentHeap
      val saved = heap(NamedAddr("x"), Str("f"))
      checkCovered(before, Map(x -> NamedAddr("x")), heap)
      val effect = Effect.Empty.fieldUpdate("g", objectTy)(using before)
      val returned = checker.transfer.instantiate(
        call,
        AbsValue(SymTy.SSym(0)),
        Map(0 -> AbsValue(argument)),
        effect,
      )(using before)
      heap.update(NamedAddr("x"), Str("g"), Str("abc"))
      val stored = before.weaken(effect).update(r, returned)
      assert(returned.symty == argument)
      val values = Map[Local, Value](x -> NamedAddr("x"), r -> saved)
      checkCovered(stored, values, heap)
      val after = assume(stored, r, NumberT)
      assert(after.get(x).ty(using after).record("f").value <= NumberT)
      checkCovered(after, values, heap)
    }
  }

  init
}
