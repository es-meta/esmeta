package esmeta.js

import esmeta.cfg.CFG
import esmeta.ir.*
import esmeta.interp.*
import esmeta.spec.*
import esmeta.js.builtin.*
import scala.collection.mutable.{Map => MMap}

class Initialize(
  cfg: CFG,
  sourceText: String,
  cachedAst: Option[Ast],
) {

  /** the result state of initialization */
  lazy val result: State = State(
    cfg,
    context = Context(cfg.main),
    sourceText = Some(sourceText),
    cachedAst = cachedAst,
    globals = initGlobal,
    heap = initHeap,
  )

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // implicit CFG
  given CFG = cfg

  // initial globals
  private lazy val initGlobal: MMap[Global, Value] = MMap(
    CONTEXT -> Null,
    SOURCE_TEXT -> Str(sourceText),
    EXECUTION_STACK -> NamedAddr(EXECUTION_STACK),
    HOST_DEFINED -> Undef,
    INTRINSICS -> NamedAddr(INTRINSICS),
    GLOBAL -> NamedAddr(GLOBAL),
    SYMBOL -> NamedAddr(SYMBOL),
    REALM -> NamedAddr(REALM),
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
  private lazy val initHeap: Heap = {
    given CFG = cfg
    val intr = Intrinsics(cfg)
    val glob = GlobalObject(cfg)
    val sym = Symbols(cfg)

    val map: MMap[Addr, Obj] = MMap(
      NamedAddr(INTRINSICS) -> intr.obj,
      NamedAddr(GLOBAL) -> glob.obj,
      NamedAddr(SYMBOL) -> sym.obj,
      NamedAddr(REALM) -> MapObj("RealmRecord"),
      NamedAddr(EXECUTION_STACK) -> ListObj(),
      NamedAddr(JOB_QUEUE) -> ListObj(),
      NamedAddr(SYMBOL_REGISTRY) -> ListObj(),
    )

    // add symbols
    map ++= sym.map

    // add intrinsics
    map ++= intr.map

    // add member functions of intrinsics
    addBaseBuiltinFuncs(map)
    addPropBuiltinFuncs(map)

    // add global object
    map ++= glob.map

    Heap(map, map.size)
  }

  // get closures
  private def clo(name: String): Clo = Clo(cfg.fnameMap(name), Map())
  private def intrClo(name: String): Clo = clo(intrName(name))

  // add member functions of intrinsics
  import BuiltinHead.Ref.*
  private def createBuiltinFunction(
    name: String,
    head: BuiltinHead,
    defaultName: String,
    map: MMap[Addr, Obj],
  ): Unit = {
    val (baseName, baseAddr) = (intrName(name), intrAddr(name))
    val subAddr = submapAddr(baseName)
    val nameAddr = descAddr(name, "name")
    val lengthAddr = descAddr(name, "length")

    val baseObj = map.get(baseAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj("BuiltinFunctionObject")
    val subMapObj = map.get(subAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj("SubMap")
    val nameMapObj = map.get(nameAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj("PropertyDescriptor")
    val lengthMapObj = map.get(lengthAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj("PropertyDescriptor")
    val defaultLength = head.params.count(_.kind == Param.Kind.Normal)

    map += baseAddr -> baseObj
      .findOrUpdate(Str("Extensible"), Bool(true))
      .findOrUpdate(Str("ScriptOrModule"), Null)
      .findOrUpdate(Str("Realm"), realmAddr)
      .findOrUpdate(Str("Code"), intrClo(name))
      .findOrUpdate(Str("Prototype"), intrAddr("Function.prototype"))
      .findOrUpdate(Str("SubMap"), subAddr)
      .findOrUpdate(Str("InitialName"), Str(defaultName))

    map += subAddr -> subMapObj
      .findOrUpdate(Str("length"), lengthAddr)
      .findOrUpdate(Str("name"), nameAddr)

    map += nameAddr -> nameMapObj
      .findOrUpdate(Str("Value"), Str(defaultName))
      .findOrUpdate(Str("Writable"), Bool(false))
      .findOrUpdate(Str("Enumerable"), Bool(false))
      .findOrUpdate(Str("Configurable"), Bool(true))

    map += lengthAddr -> lengthMapObj
      .findOrUpdate(Str("Value"), Number(defaultLength))
      .findOrUpdate(Str("Writable"), Bool(false))
      .findOrUpdate(Str("Enumerable"), Bool(false))
      .findOrUpdate(Str("Configurable"), Bool(true))
  }
  private def addBaseBuiltinFuncs(map: MMap[Addr, Obj]): Unit = for {
    func <- cfg.funcs
    (name, head) <- func.head match
      case Some(head: BuiltinHead) =>
        head.ref match
          case b: NormalBase    => Some((b.toString, head))
          case b: IntrinsicBase => Some((b.toString, head))
          case _                => None
      case _ => None
  } createBuiltinFunction(name, head, name, map)
  private def addPropBuiltinFuncs(map: MMap[Addr, Obj]): Unit = for {
    func <- cfg.funcs
    head <- func.head match
      case Some(head: BuiltinHead) => Some(head)
      case _                       => None
    (base, prop, propV, propName) <- head.ref match
      case NormalAccess(base, name) =>
        Some(base.toString(), name, Str(name), name)
      case SymbolAccess(base, name) =>
        Some(
          base.toString(),
          name,
          symbolAddr(name),
          s"[Symbol.$name]",
        )
      case _ => None // TODO getter & setter
    baseMapObj <- map.get(submapAddr(intrName(base))) match
      case Some(m: MapObj) => Some(m)
      case _               => None
    name <- propV match
      case Str(_)       => Some(s"$base.$prop")
      case NamedAddr(_) => Some(s"$base[@@$prop]")
      case _            => None
    desc = descAddr(base, prop)
  } {
    baseMapObj.update(propV, desc)
    map.getOrElse(
      desc,
      map += desc -> MapObj("PropertyDescriptor")(
        Str("Value") -> intrAddr(name),
        Str("Writable") -> Bool(true),
        Str("Enumerable") -> Bool(false),
        Str("Configurable") -> Bool(true),
      ),
    )
    createBuiltinFunction(name, head, propName, map)
  }

  // get length value from built-in head parameters
  private def getLength(params: List[Param]): Int =
    params.count(_.kind == Param.Kind.Normal)
}
object Initialize {
  def apply(
    cfg: CFG,
    sourceText: String,
    cachedAst: Option[Ast] = None,
  ): State = new Initialize(cfg, sourceText, cachedAst).result
}
