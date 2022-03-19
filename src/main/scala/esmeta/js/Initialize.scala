package esmeta.js

import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.ir.*
import esmeta.js.builtin.*
import esmeta.spec.*
import esmeta.spec.util.{Parser => SpecParser}
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
  extension (str: String) {
    def getData: Option[(String, String, PureValue, String, Boolean)] =
      SpecParser.parseBuiltinRef(str).getData
  }
  extension (ref: BuiltinHead.Ref) {
    def getData: Option[(String, String, PureValue, String, Boolean)] =
      ref match
        case NormalAccess(b, n) if !(yets contains b.toString) =>
          Some((b.toString, n, Str(n), n, true))
        case SymbolAccess(b, n) if !(yets contains b.toString) =>
          Some((b.toString, s"@@$n", symbolAddr(n), s"[Symbol.$n]", true))
        case Getter(ref) =>
          ref.getData match
            case Some((base, prop, propV, propName, _)) =>
              Some((base, prop, propV, s"get $propName", false))
            case _ => None
        case _ => None // TODO setter
  }

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
          case Base(b) if !(yets contains b) => Some((b, head))
          case _                             => None
      case _ => None
  } createBuiltinFunction(name, getLength(head), name, map)
  private def addPropBuiltinFuncs(map: MMap[Addr, Obj]): Unit = for {
    func <- cfg.funcs if func.irFunc.kind == Func.Kind.Builtin
    fname = func.name.stripPrefix("INTRINSICS.")
    (base, prop, propV, defaultName, isData) <- fname.getData
    baseMapObj <- map.get(submapAddr(intrName(base))) match
      case Some(m: MapObj) => Some(m)
      case _               => None
  } {
    val desc = descAddr(base, prop)
    val defaultLength = func.head.fold(0)(getLength(_))
    baseMapObj.update(propV, desc)
    if (isData) // data property
      map.getOrElse(
        desc,
        map += desc -> DataProperty(intrAddr(fname), T, F, T).toObject,
      )
    else // accessor property
      map.getOrElse(
        desc,
        map += desc -> AccessorProperty(intrAddr(fname), U, F, T).toObject,
      )
    createBuiltinFunction(fname, defaultLength, defaultName, map)
  }

  // get length value from built-in head parameters
  private def getLength(head: Head): Int =
    head.originalParams.count(_.kind == Param.Kind.Normal)
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
