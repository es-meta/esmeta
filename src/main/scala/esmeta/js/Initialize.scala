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

    val map: MMap[Addr, Obj] = MMap(
      NamedAddr(INTRINSICS) -> intr.obj,
      NamedAddr(GLOBAL) -> glob.obj,
      NamedAddr(REALM) -> MapObj("RealmRecord"),
      NamedAddr(EXECUTION_STACK) -> ListObj(),
      NamedAddr(JOB_QUEUE) -> ListObj(),
      NamedAddr(SYMBOL_REGISTRY) -> ListObj(),
    )

    // add intrinsics
    map ++= intr.map

    // add member functions of intrinsics
    addIntrinsicFunc(map)

    // add global object
    map ++= glob.map

    Heap(map, map.size)
  }

  // add member functions of intrinsics
  import BuiltinHead.Ref.*
  private def addIntrinsicFunc(map: MMap[Addr, Obj]): Unit = for {
    func <- cfg.funcs
    // TODO handle INTRINSICS.AsyncFunction
    head <- func.head match
      case Some(head: BuiltinHead) => Some(head)
      case _                       => None
    (base, prop, propV, propName) <- head.ref match
      case NormalBase(name) =>
        map.getOrElse(NamedAddr(s"$INTRINSICS.$name"), None) match
          case YetObj(tyname, desc) => None
          case _                    => Some(INTRINSICS, name, Str(name), name)
      case NormalAccess(base, name) =>
        Some(s"$INTRINSICS.$base", name, Str(name), name)
      case SymbolAccess(base, name) =>
        val addr = NamedAddr(s"$INTRINSICS.Symbol.$name")
        Some(
          s"$INTRINSICS.$base",
          name,
          addr,
          s"[Symbol.$name]",
        )
      case _ => None
    baseAddr = NamedAddr(s"$base.SubMap")
    baseMapObj <- map.get(baseAddr) match
      case Some(m: MapObj) => Some(m)
      case _               => None
    name <- propV match
      case Str(name)       => Some(s"$base.$prop")
      case NamedAddr(name) => Some(s"$base[$prop]")
      case _               => None
    addr = NamedAddr(s"$name")
    desc = NamedAddr(s"DESC:$name")
  } {
    baseMapObj.update(propV, desc)
    map.getOrElse(
      desc,
      map += desc -> MapObj("PropertyDescriptor")(
        Str("Value") -> addr,
        Str("Writable") -> Bool(true),
        Str("Enumerable") -> Bool(false),
        Str("Configurable") -> Bool(true),
      ),
    )

    val subAddr = submapAddr(name)
    val nameAddr = descAddr(name, "name")
    val lengthAddr = descAddr(name, "length")

    val mapObj = map.get(addr) match
      case Some(m: MapObj) => m
      case _               => MapObj("BuiltinFunctionObject")
    val nameMapObj = map.get(nameAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj("PropertyDescriptor")
    val subMapObj = map.get(subAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj("SubMap")
    val lengthMapObj = map.get(lengthAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj("PropertyDescriptor")

    val initName = nameMapObj.props
      .get(Str("Value"))
      .fold(Str(propName))(_.value)

    map += addr -> mapObj
      .findOrUpdate(Str("Extensible"), Bool(true))
      .findOrUpdate(Str("ScriptOrModule"), Null)
      .findOrUpdate(Str("Realm"), realmAddr)
      .findOrUpdate(Str("Code"), Clo(func, Map()))
      .findOrUpdate(Str("Prototype"), intrAddr("Function.prototype"))
      .findOrUpdate(Str("SubMap"), subAddr)
      .findOrUpdate(Str("InitialName"), initName)

    map += subAddr -> subMapObj
      .findOrUpdate(Str("name"), nameAddr)
      .findOrUpdate(Str("length"), lengthAddr)

    map += nameAddr -> nameMapObj
      .findOrUpdate(Str("Value"), Str(propName))
      .findOrUpdate(Str("Writable"), Bool(false))
      .findOrUpdate(Str("Enumerable"), Bool(false))
      .findOrUpdate(Str("Configurable"), Bool(true))

    map += lengthAddr -> lengthMapObj
      .findOrUpdate(Str("Value"), Number(getLength(head.params)))
      .findOrUpdate(Str("Writable"), Bool(false))
      .findOrUpdate(Str("Enumerable"), Bool(false))
      .findOrUpdate(Str("Configurable"), Bool(true))
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
