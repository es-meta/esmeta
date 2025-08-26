package esmeta.es

import esmeta.cfg.*
import esmeta.es.builtin.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap}

class Initialize(cfg: CFG) {
  import cfg.*

  /** get initial state from source text */
  def from(sourceText: String): State =
    val (ast, semiInjected) = cfg.scriptParser.fromWithCode(sourceText)
    from(semiInjected, ast)

  /** get initial state from script */
  def from(script: Script): State = from(script.code)

  /** get initial state from JS file */
  def fromFile(filename: String): State =
    val (ast, semiInjected) = cfg.scriptParser.fromFileWithCode(filename)
    from(semiInjected, ast, Some(filename))

  /** get initial state with source text and cached AST */
  def from(
    sourceText: String,
    ast: Ast,
    filename: Option[String] = None,
  ): State = State(
    cfg,
    context = Context(cfg.main),
    sourceText = Some(sourceText),
    filename = filename,
    cachedAst = Some(ast),
    globals = MMap.from(initGlobal + (Global(SOURCE_TEXT) -> Str(sourceText))),
    heap = initHeap.copied,
  )

  // initial globals
  lazy val initGlobal: Map[Global, Value] =
    initTypedGlobal.map { case (k, (v, _)) => k -> v }
  lazy val initTypedGlobal: Map[Global, (Value, Ty)] = Map(
    EXECUTION_STACK ->
    (NamedAddr(EXECUTION_STACK), ListT(RecordT("ExecutionContext"))),
    SOURCE_TEXT -> (Str(""), StrT),
    HOST_DEFINED -> (Undef, UndefT),
    INTRINSICS -> (NamedAddr(INTRINSICS), UnknownTy()),
    TYPED_ARRAY -> (NamedAddr(TYPED_ARRAY), MapT(StrT, taType)),
    GLOBAL -> (NamedAddr(GLOBAL), UnknownTy()),
    SYMBOL -> (NamedAddr(SYMBOL), sym.ty),
    MATH_PI -> (Math(scala.math.Pi), MathT),
    AGENT_RECORD -> (NamedAddr(AGENT_RECORD), RecordT("AgentRecord")),
    AGENT_SIGNIFIER -> (NamedAddr(AGENT_SIGNIFIER), StrT("AgentSignifier")),
    CANDIDATE_EXECUTION -> (
      NamedAddr("CandidateExecution"),
      RecordT("CandidateExecution"),
    ),
    KEPT_ALIVE -> (NamedAddr("KeptAlive"), ListT(ObjectT || SymbolT)),
    REALM -> (NamedAddr(REALM), RecordT("RealmRecord")),
    JOB_QUEUE -> (NamedAddr(JOB_QUEUE), ListT(RecordT("JobRecord"))),
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
      NamedAddr(GLOBAL) -> glob.obj,
      NamedAddr(SYMBOL) -> sym.obj,
      NamedAddr(AGENT_RECORD) -> recordObj("AgentRecord")(
        "LittleEndian" -> Bool(true),
        "CanBlock" -> Bool(true),
        "Signifier" -> NamedAddr(AGENT_SIGNIFIER),
        "IsLockFree1" -> Bool(true),
        "IsLockFree2" -> Bool(true),
        "IsLockFree8" -> Bool(true),
        "CandidateExecution" -> NamedAddr(CANDIDATE_EXECUTION),
        "KeptAlive" -> NamedAddr(KEPT_ALIVE),
      ),
      NamedAddr(AGENT_SIGNIFIER) -> recordObj("AgentSignifier"),
      NamedAddr(CANDIDATE_EXECUTION) -> YetObj(
        "CandidateExecution",
        "AgentRecord.[[CandidateExecution]]",
      ),
      NamedAddr(KEPT_ALIVE) -> ListObj(),
      NamedAddr(REALM) -> recordObj("RealmRecord"),
      NamedAddr(EXECUTION_STACK) -> ListObj(),
      NamedAddr(JOB_QUEUE) -> ListObj(),
      NamedAddr(SYMBOL_REGISTRY) -> ListObj(),
    )

    // add symbols
    map ++= sym.map

    // add intrinsic built-in objects
    map ++= intrHeap

    // add member functions of intrinsics
    addBaseBuiltinFuncs(map)
    addPropBuiltinFuncs(map)

    // add global object
    map ++= glob.map

    Heap(map)
  }

  /** names of intrinsic built-in objects */
  lazy val intrNames = intr.replacedModels.map(_.name).toSet ++ yets.keySet

  /** intrinsic object */
  lazy val intrObj = MapObj(
    intrNames.toList.map(x => Str(s"%$x%") -> intrAddr(x)),
  )

  /** names of typed array constructors */
  lazy val taInstances = intr.getInstances("TypedArray")

  /** names of typed array constructors */
  lazy val taNames = taInstances.keys.toList.sorted

  /** type of typed array constructors */
  lazy val taType = RecordT(
    "",
    Map(
      "Intrinsic" -> ObjectT,
      "ElementType" -> EnumT,
      "ElementSize" -> NonNegIntT,
      "ConversionOperation" -> CloT,
    ),
  )

  /** information of typed array constructors */
  lazy val taObj = MapObj(taNames.map(x => Str(x) -> taAddr(x)))

  /** names of intrinsic built-in objects */
  lazy val intrHeap = {
    import intr.*

    var _map = Map[Addr, Obj](
      NamedAddr(INTRINSICS) -> intrObj,
      NamedAddr(TYPED_ARRAY) -> taObj,
    )

    // add intrinsic objects
    for {
      Model(name, tname, imap, nmap) <- replacedModels
      base = name.split("\\.").head
      if !yets.contains(base)
      addr @ NamedAddr(iname) = intrAddr(name)
    } {
      // base object
      _map += addr -> recordObj(tname)(
        (INNER_MAP -> mapAddr(iname)) ::
        (PRIVATE_ELEMENTS -> elemsAddr(iname)) ::
        imap.map { (k, v) => k -> toValue(v) },
      )
      // inner map object
      _map ++= getMapObjects(iname, name, nmap.map { _ -> toDesc(_) })
    }

    // add typed array constructors
    for {
      (x, m) <- taInstances
      addr = taAddr(x)
      record = RecordObj("", m.map { (k, v) => k -> toValue(v) })
    } {
      _map += addr -> record
    }

    // not yet objects
    yets.foreach { (name, _) =>
      _map += (intrAddr(name) -> YetObj(name, name))
    }

    // result
    _map
  }

  /** types of intrinsic built-in objects */
  lazy val intrTypes: Map[String, ValueTy] =
    val xs = (for {
      x <- intrNames.toList
      addr = intrAddr(x)
      case (obj: RecordObj) <- intrHeap.get(addr)
      ty =
        if (obj.map contains "Construct") ConstructorT
        else if (obj.map contains "Call") FunctionT
        else ObjectT
    } yield x -> ty).toMap ++ yets
    xs.map { case (x, ty) => s"%$x%" -> ty }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // implicit CFG
  given CFG = cfg

  val intr = cfg.intrinsics
  val glob = GlobalObject(cfg)
  val sym = builtin.Symbol(cfg)

  // get closures
  private def clo(name: String): Clo = Clo(cfg.fnameMap(name), Map())
  private def intrClo(name: String): Clo = clo(intrName(name))

  case class Data(
    base: String,
    prop: PropKey,
    value: Value,
    defaultName: String,
    isData: Boolean,
    isGetter: Boolean,
  )
  object Data {
    def get(func: Func): Option[Data] = func.head match
      case Some(BuiltinHead(path, _, _)) => get(path)
      case _ => get(BuiltinPath.from(func.name.stripPrefix("INTRINSICS.")))
    def get(path: BuiltinPath): Option[Data] =
      import BuiltinPath.*
      path match
        case NormalAccess(b, n) if !(yets contains b.toString) =>
          Some(Data(b.toString, PropKey.Str(n), Str(n), n, T, F))
        case SymbolAccess(b, n) if !(yets contains b.toString) =>
          Some(
            Data(
              b.toString,
              PropKey.Sym(n),
              symbolAddr(n),
              s"[Symbol.$n]",
              T,
              F,
            ),
          )
        case Getter(path) =>
          get(path).map {
            case Data(base, prop, propV, propName, _, _) =>
              Data(base, prop, propV, s"get $propName", F, T)
          }
        case Setter(path) =>
          get(path).map {
            case Data(base, prop, propV, propName, _, _) =>
              Data(base, prop, propV, s"set $propName", F, F)
          }
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
    val subAddr = mapAddr(baseName)
    val listAddr = elemsAddr(baseName)
    val nameAddr = descAddr(name, PropKey.Str("name"))
    val lengthAddr = descAddr(name, PropKey.Str("length"))

    val baseObj = map.get(baseAddr) match
      case Some(r: RecordObj) => r
      case _                  => recordObj("BuiltinFunctionObject")
    val mapObj = map.get(subAddr) match
      case Some(m: MapObj) => m
      case _               => MapObj()
    val listObj = map.get(listAddr) match
      case Some(m: ListObj) => m
      case _                => ListObj()
    val namerecordObj = map.get(nameAddr) match
      case Some(r: RecordObj) => r
      case _                  => recordObj("PropertyDescriptor")
    val lengthrecordObj = map.get(lengthAddr) match
      case Some(r: RecordObj) => r
      case _                  => recordObj("PropertyDescriptor")

    def updateRecord(obj: RecordObj)(
      pairs: (String, Value)*,
    ): obj.type =
      for { (f, v) <- pairs if !obj.map.contains(f) } obj.update(Str(f), v)
      obj

    def newMap(obj: MapObj)(pairs: (Value, Value)*): MapObj =
      val newObj = MapObj(pairs)
      for { (f, v) <- obj.map } newObj.update(f, v)
      newObj

    intrObj.map += Str(s"%$name%") -> baseAddr

    map += baseAddr -> updateRecord(baseObj)(
      "Extensible" -> Bool(true),
      "ScriptOrModule" -> Null,
      "Realm" -> realmAddr,
      "Prototype" -> intrAddr("Function.prototype"),
      "InitialName" -> Str(defaultName),
      INNER_CODE -> intrClo(name),
      INNER_MAP -> subAddr,
      PRIVATE_ELEMENTS -> listAddr,
    )

    map += subAddr -> newMap(mapObj)(
      Str("length") -> lengthAddr,
      Str("name") -> nameAddr,
    )

    map += listAddr -> listObj

    map += lengthAddr -> updateRecord(lengthrecordObj)(
      "Value" -> Number(defaultLength),
      "Writable" -> Bool(false),
      "Enumerable" -> Bool(false),
      "Configurable" -> Bool(true),
    )

    map += nameAddr -> updateRecord(namerecordObj)(
      "Value" -> Str(defaultName),
      "Writable" -> Bool(false),
      "Enumerable" -> Bool(false),
      "Configurable" -> Bool(true),
    )
  }
  private def addBaseBuiltinFuncs(map: MMap[Addr, Obj]): Unit =
    import BuiltinPath.*
    for { func <- cfg.funcs } func.head match
      case Some(head @ BuiltinHead(Base(x), _, _)) if !yets.contains(x) =>
        createBuiltinFunction(x, getLength(head), x, map)
      case _ => None
  private def addPropBuiltinFuncs(map: MMap[Addr, Obj]): Unit =
    var propMap: Map[Addr, PropDesc] = Map()
    for {
      func <- cfg.funcs if func.kind == FuncKind.Builtin
      fname = func.name.stripPrefix("INTRINSICS.")
      Data(base, prop, propV, defaultName, isData, isGetter) <- Data.get(func)
      baseMapObj <- map.get(mapAddr(intrName(base))) match
        case Some(m: MapObj) => Some(m)
        case _               => None
      daddr = descAddr(base, prop)
      defaultLength = func.head.fold(0)(getLength(_))
      _ = baseMapObj.update(propV, daddr)
      iaddr = intrAddr(fname)
      desc =
        if (isData) DataDesc(iaddr, T, F, T)
        else if (isGetter) AccessorDesc(iaddr, U, F, T)
        else AccessorDesc(U, iaddr, F, T)
      _ =
        if (yetFuncs contains fname) map += (iaddr -> YetObj("", fname))
        else createBuiltinFunction(fname, defaultLength, defaultName, map)
    } (propMap.get(daddr), desc) match
      case (Some(l: AccessorDesc), r: AccessorDesc) =>
        var ap: AccessorDesc = l
        if (l.get == U) ap = ap.copy(get = r.get)
        if (l.set == U) ap = ap.copy(set = r.set)
        propMap += daddr -> ap
      case _ =>
        propMap += daddr -> desc
    for {
      (daddr, desc) <- propMap
    } map.getOrElse(daddr, map += daddr -> toObject(desc))

  // get length value from built-in head parameters
  private def getLength(head: Head): Int =
    head.originalParams.count(_.kind == ParamKind.Normal)

  /** convert string to value */
  private val toValue = stateParser.parseBy(stateParser.value)

  /** convert string to property descriptor */
  private val descParser = DataDesc.Parser(cfg)
  private val toDesc = descParser.parseBy(descParser.propDesc)
}
