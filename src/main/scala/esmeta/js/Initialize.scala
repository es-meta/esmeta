package esmeta.js

import esmeta.cfg.CFG
import esmeta.ir.*
import esmeta.interp.*
import esmeta.spec.*
import esmeta.js.builtin.*
import scala.collection.mutable.{Map => MMap}

object Initialize {
  def apply(cfg: CFG, sourceText: String): State = State(
    cfg,
    context = Context(cfg.main),
    sourceText = Some(sourceText),
    cachedAst = Some(cfg.jsParser("Script", Nil).from(sourceText)),
    globals = initGlobal(sourceText),
    heap = initHeap(cfg),
  )

  // initial globals
  private def initGlobal(sourceText: String): MMap[Global, Value] = MMap(
    CONTEXT -> Null,
    SOURCE_TEXT -> Str(sourceText),
    EXECUTION_STACK -> NamedAddr(EXECUTION_STACK),
    HOST_DEFINED -> Undef,
    INTRINSICS -> NamedAddr(INTRINSICS),
    GLOBAL_OBJECT -> NamedAddr(GLOBAL_OBJECT),
    JOB_QUEUE -> NamedAddr(JOB_QUEUE),
    SYMBOL_REGISTRY -> NamedAddr(SYMBOL_REGISTRY),
    UNDEF_TYPE -> Str("Undefined"),
    NULL_TYPE -> Str("Null"),
    BOOL_TYPE -> Str("Boolean"),
    STRING_TYPE -> Str("String"),
    SYMBOL_TYPE -> Str("Symbol"),
    NUMBER_TYPE -> Str("Number"),
    BIGINT_TYPE -> Str("BigInt"),
    OBJECT_TYPE -> Str("Object"),
  ).map { case (k, v) => Global(k) -> v }

  // initial heaps
  private def initHeap(cfg: CFG): Heap = {
    val intr = Intrinsics(cfg)
    val glob = GlobalObject(cfg)

    val map: MMap[Addr, Obj] = MMap(
      NamedAddr(INTRINSICS) -> intr.obj,
      NamedAddr(GLOBAL_OBJECT) -> glob.obj,
      NamedAddr(EXECUTION_STACK) -> ListObj(),
      NamedAddr(JOB_QUEUE) -> ListObj(),
      NamedAddr(SYMBOL_REGISTRY) -> ListObj(),
    )

    // add intrinsics
    // TODO add more intrinsics modeling
    map ++= intr.map

    // add global object
    map ++= glob.map

    Heap(map, map.size)
  }

}
