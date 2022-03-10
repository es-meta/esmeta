package esmeta.js

import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.ir.*
import esmeta.js.builtin.*
import esmeta.spec.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap}
import esmeta.test262.*

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

  import BuiltinHead.Ref.*

  // get data from builtin head
  private def getBuiltinData(
    ref: BuiltinHead.Ref,
  ): Option[(String, String, PureValue, String, Boolean)] = ref match
    case NormalAccess(base, name) =>
      Some((base.toString(), name, Str(name), name, true))
    case SymbolAccess(base, name) =>
      Some((base.toString(), name, symbolAddr(name), s"[Symbol.$name]", true))
    case Getter(ref) =>
      getBuiltinData(ref) match
        case Some((base, prop, propV, propName, _)) =>
          Some((base, prop, propV, s"get $propName", false))
        case _ => None
    case _ => None // TODO setter

  // add member functions of intrinsics
  private def createBuiltinFunction(
    name: String,
    defaultLength: Int,
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
  } createBuiltinFunction(name, getLength(head), name, map)
  private def addPropBuiltinFuncs(map: MMap[Addr, Obj]): Unit = for {
    func <- cfg.funcs
    head <- func.head match
      case Some(head: BuiltinHead) => Some(head)
      case _                       => None
    (base, prop, propV, defaultName, isData) <- getBuiltinData(head.ref)
    baseMapObj <- map.get(submapAddr(intrName(base))) match
      case Some(m: MapObj) => Some(m)
      case _               => None
    name = head.ref.toString
    desc = descAddr(base, prop)
  } {
    baseMapObj.update(propV, desc)
    if (isData) // data property
      map.getOrElse(
        desc,
        map += desc -> MapObj("PropertyDescriptor")(
          Str("Value") -> intrAddr(name),
          Str("Writable") -> Bool(true),
          Str("Enumerable") -> Bool(false),
          Str("Configurable") -> Bool(true),
        ),
      )
    else // accessor property
      map.getOrElse(
        desc,
        map += desc -> MapObj("PropertyDescriptor")(
          Str("Get") -> intrAddr(name),
          Str("Set") -> Undef, // TODO handle setter
          Str("Enumerable") -> Bool(false),
          Str("Configurable") -> Bool(true),
        ),
      )
    createBuiltinFunction(name, getLength(head), defaultName, map)
  }

  // get length value from built-in head parameters
  private def getLength(head: BuiltinHead): Int =
    head.params.count(_.kind == Param.Kind.Normal)
}
object Initialize {
  def apply(
    cfg: CFG,
    sourceText: String,
    cachedAst: Option[Ast] = None,
  ): State = new Initialize(cfg, sourceText, cachedAst).result

  /** initialize from file */
  def fromFile(cfg: CFG, filename: String, test262: Boolean = false): State =
    if (!test262) apply(cfg, readFile(filename))
    else {
      val test262 = Test262(cfg.spec)
      val (sourceText, ast) = test262.loadTestFromFile(filename)
      apply(cfg, sourceText, Some(ast))
    }
}
