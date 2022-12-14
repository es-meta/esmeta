package esmeta.es

import esmeta.cfg.CFG
import esmeta.es.builtin.*
import esmeta.ir.*
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap}

class Initialize(cfg: CFG) {

  /** the result state of initialization */
  def getResult(
    sourceText: String,
    cachedAst: Option[Ast],
    filename: Option[String],
  ): State = State(
    cfg,
    context = Context(cfg.main),
    sourceText = Some(sourceText),
    cachedAst = cachedAst,
    filename = filename,
    globals = MMap.from(initGlobal + (Global(SOURCE_TEXT) -> Str(sourceText))),
    heap = initHeap,
  )

  // initial globals
  lazy val initGlobal: Map[Global, Value] =
    initTypedGlobal.map { case (k, (v, _)) => k -> v }
  lazy val initTypedGlobal: Map[Global, (Value, Ty)] = Map(
    EXECUTION_STACK ->
    (NamedAddr(EXECUTION_STACK), ListT(NameT("ExecutionContext"))),
    HOST_DEFINED -> (Undef, UndefT),
    INTRINSICS -> (NamedAddr(INTRINSICS), UnknownTy()),
    GLOBAL -> (NamedAddr(GLOBAL), UnknownTy()),
    SYMBOL -> (NamedAddr(SYMBOL), sym.ty),
    MATH_PI -> (Math(scala.math.Pi), MathT),
    REALM -> (NamedAddr(REALM), NameT("RealmRecord")),
    JOB_QUEUE -> (NamedAddr(JOB_QUEUE), ListT(NameT("PendingJob"))),
    SYMBOL_REGISTRY -> (NamedAddr(SYMBOL_REGISTRY), UnknownTy()),
    UNDEF_TYPE -> (Str("Undefined"), StrT("Undefined")),
    NULL_TYPE -> (Str("Null"), StrT("Null")),
    BOOL_TYPE -> (Str("Boolean"), StrT("Boolean")),
    STRING_TYPE -> (Str("String"), StrT("String")),
    SYMBOL_TYPE -> (Str("Symbol"), StrT("Symbol")),
    NUMBER_TYPE -> (Str("Number"), StrT("Number")),
    BIGINT_TYPE -> (Str("BigInt"), StrT("BigInt")),
    OBJECT_TYPE -> (Str("Object"), StrT("Object")),
  ).map { case (k, p) => Global(k) -> p }

  // initial heaps
  lazy val initHeap: Heap = {

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

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // implicit CFG
  given CFG = cfg

  private val intr = Intrinsics(cfg)
  private val glob = GlobalObject(cfg)
  private val sym = builtin.Symbol(cfg)

  // get closures
  private def clo(name: String): Clo = Clo(cfg.fnameMap(name), Map())
  private def intrClo(name: String): Clo = clo(intrName(name))

  // get data from builtin head
  extension (str: String) {
    def getData: Option[(String, String, PureValue, String, Boolean, Boolean)] =
      BuiltinPath.from(str).getData
  }
  extension (path: BuiltinPath) {
    def getData: Option[(String, String, PureValue, String, Boolean, Boolean)] =
      import BuiltinPath.*
      path match
        case NormalAccess(b, n) if !(yets contains b.toString) =>
          Some((b.toString, n, Str(n), n, T, F))
        case SymbolAccess(b, n) if !(yets contains b.toString) =>
          Some((b.toString, s"@@$n", symbolAddr(n), s"[Symbol.$n]", T, F))
        case Getter(path) =>
          path.getData match
            case Some((base, prop, propV, propName, _, _)) =>
              Some((base, prop, propV, s"get $propName", F, T))
            case _ => None
        case Setter(path) =>
          path.getData match
            case Some((base, prop, propV, propName, _, _)) =>
              Some((base, prop, propV, s"set $propName", F, F))
            case _ => None
        case _ => None
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
        head.path match
          case BuiltinPath.Base(b) if !(yets contains b) => Some((b, head))
          case _                                         => None
      case _ => None
  } createBuiltinFunction(name, getLength(head), name, map)
  private def addPropBuiltinFuncs(map: MMap[Addr, Obj]): Unit =
    var propMap: Map[Addr, Property] = Map()
    for {
      func <- cfg.funcs if func.irFunc.kind == FuncKind.Builtin
      fname = func.name.stripPrefix("INTRINSICS.")
      (base, prop, propV, defaultName, isData, isGetter) <- fname.getData
      baseMapObj <- map.get(submapAddr(intrName(base))) match
        case Some(m: MapObj) => Some(m)
        case _               => None
      desc = descAddr(base, prop)
      defaultLength = func.head.fold(0)(getLength(_))
      _ = baseMapObj.update(propV, desc)
      intr = intrAddr(fname)
      property =
        if (isData) DataProperty(intr, T, F, T)
        else if (isGetter) AccessorProperty(intr, U, F, T)
        else AccessorProperty(U, intr, F, T)
      _ =
        if (yetFuncs contains fname) map += (intr -> YetObj("", fname))
        else createBuiltinFunction(fname, defaultLength, defaultName, map)
    } (propMap.get(desc), property) match
      case (Some(l: AccessorProperty), r: AccessorProperty) =>
        var ap: AccessorProperty = l
        if (l.get == U) ap = ap.copy(get = r.get)
        if (l.set == U) ap = ap.copy(set = r.set)
        propMap += desc -> ap
      case _ =>
        propMap += desc -> property
    for {
      (desc, property) <- propMap
    } map.getOrElse(desc, map += desc -> property.toObject)

  // get length value from built-in head parameters
  private def getLength(head: Head): Int =
    head.originalParams.count(_.kind == ParamKind.Normal)
}
object Initialize {
  def apply(
    cfg: CFG,
    sourceText: String,
    cachedAst: Option[Ast] = None,
    filename: Option[String] = None,
  ): State = new Initialize(cfg).getResult(sourceText, cachedAst, filename)

  /** initialize from file */
  def fromFile(cfg: CFG, filename: String): State =
    apply(cfg, readFile(filename), filename = Some(filename))
}
