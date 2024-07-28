package esmeta.injector

import esmeta.INJECT_LOG_DIR
import esmeta.cfg.CFG
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.es.*
import esmeta.es.builtin.INNER_MAP
import esmeta.spec.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{LINE_SEP, RESOURCE_DIR}
import java.io.PrintWriter
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.TimeoutException

/** assertion injector */
object Injector:
  def apply(
    cfg: CFG,
    src: String,
    defs: Boolean = false,
    log: Boolean = false,
  ): String =
    val extractor = ExitStateExtractor(Initialize(cfg, src))
    new Injector(extractor, defs, log).result

  /** injection from files */
  def fromFile(
    cfg: CFG,
    filename: String,
    defs: Boolean = false,
    log: Boolean = false,
  ): String =
    val extractor = ExitStateExtractor(Initialize.fromFile(cfg, filename))
    new Injector(extractor, defs, log).result

  /** assertion definitions */
  lazy val assertions: String = readFile(s"$RESOURCE_DIR/assertions.js")

/** extensible helper of assertion injector */
class Injector(
  extractor: ExitStateExtractor,
  defs: Boolean,
  log: Boolean,
) {

  /** injected script */
  lazy val result: String =
    if (defs) app >> Injector.assertions >> LINE_SEP
    app >> "// [EXIT] " >> exitTag.toString // append exit status tag
    app :> original // append original script
    if (normalExit)
      if (isAsync) startAsync // handle async
      handleVariable // inject assertions from variables
      handleLet // inject assertions from lexical variables
      if (isAsync) endAsync
    if (log)
      pw.close
      println("[Injector] Logging finished")
    app.toString

  /** initial state */
  lazy val initSt: State = extractor.initSt

  /** exit state */
  lazy val exitSt: State = extractor.result

  /** original script */
  lazy val original = initSt.sourceText.get

  /** exit status tag */
  lazy val exitTag: ExitTag = ExitTag(exitSt)

  /** normal termination */
  lazy val normalExit: Boolean = exitTag == NormalTag

  /** check whether it uses asynchronous features */
  // TODO more precise detection
  lazy val isAsync: Boolean =
    original.contains("async") || original.contains("Promise")

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // logging
  private lazy val pw: PrintWriter =
    println(s"[Injector] Logging into $INJECT_LOG_DIR...")
    mkdir(INJECT_LOG_DIR)
    getPrintWriter(s"$INJECT_LOG_DIR/log")
  private def log(data: Any): Unit = if (log) { pw.println(data); pw.flush() }
  private def warning(msg: String): Unit = log(s"[Warning] $msg")

  // appender
  private val app: Appender = new Appender

  // handle async
  private def startAsync: Unit =
    log("handling async..."); app :> "$delay(() => {"
  private def endAsync: Unit =
    app :> "});"

  // handle variables
  private def handleVariable: Unit = for (x <- createdVars.toList.sorted) {
    log("handling variable...")
    getValue(s"""$globalMap["$x"].Value""") match
      case Absent => /* do nothing(handle global accessor property) */
      case sv: SimpleValue =>
        app :> s"$$assert.sameValue($x, ${sv2str(sv)});"
      case addr: Addr => handleObject(addr, x)
      case _          => /* do nothing */
  }

  // get created variables
  private lazy val globalMap = s"@REALM.GlobalObject.$INNER_MAP"
  private lazy val globalThis =
    getValue(s"$globalMap.globalThis.Value")
  private lazy val createdVars: Set[String] =
    val initial = getStrKeys(getValue(s"@GLOBAL.$INNER_MAP"), "<global>")
    val current = getStrKeys(getValue(globalMap), "<global>")
    current -- initial

  // handle lexical variables
  private def handleLet: Unit = for (x <- createdLets.toList.sorted) {
    log("handling let...")
    getValue(s"""$lexRecord["$x"].BoundValue""") match
      case sv: SimpleValue => app :> s"$$assert.sameValue($x, ${sv2str(sv)});"
      case addr: Addr      => handleObject(addr, x)
      case _               => /* do nothing */
  }

  // handle addresses
  private def handleObject(addr: Addr, path: String): Unit =
    log(s"handleObject: $addr, $path")
    (addr, handledObjects.get(addr)) match
      case (_, Some(origPath)) =>
        app :> s"$$assert.sameValue($path, $origPath);"
      case (_: DynamicAddr, None) if addr != globalThis =>
        handledObjects += addr -> path
        extractor.addrNames
          .get(addr)
          .map(name => app :> s"""$$algo.set($path, "$name")""")
        exitSt(addr) match
          case (_: MapObj) =>
            handlePrototype(addr, path)
            handleExtensible(addr, path)
            handleCall(addr, path)
            handleConstruct(addr, path)
            handlePropKeys(addr, path)
            handleProperty(addr, path)
          case (_: RecordObj) =>
            handlePrototype(addr, path)
            handleExtensible(addr, path)
            handleCall(addr, path)
            handleConstruct(addr, path)
            handlePropKeys(addr, path)
            handleProperty(addr, path)
          case _ =>
      case _ =>
  private var handledObjects: Map[Addr, String] = (for {
    addr <- initSt.heap.map.keySet
    name <- addrToName(addr)
  } yield addr -> name).toMap
  private lazy val PREFIX_INTRINSIC = "INTRINSICS."
  private def addrToName(addr: Addr): Option[String] = addr match
    case a @ NamedAddr(name) if name.startsWith(PREFIX_INTRINSIC) =>
      Some(name.substring(PREFIX_INTRINSIC.length))
    case _ => None

  // handle [[Prototype]]
  private def handlePrototype(addr: Addr, path: String): Unit =
    log(s"handlePrototype: $addr, $path")
    access(addr, Str("Prototype")) match
      case addr: Addr => handleObject(addr, s"Object.getPrototypeOf($path)")
      case _          => warning("non-address [[Prototype]]: $path")

  // handle [[Extensible]]
  private def handleExtensible(addr: Addr, path: String): Unit =
    log(s"handleExtensible: $addr, $path")
    access(addr, Str("Extensible")) match
      case Bool(b) =>
        app :> s"$$assert.sameValue(Object.isExtensible($path), $b);"
      case _ => warning("non-boolean [[Extensible]]: $path")

  // handle [[Call]]
  private def handleCall(addr: Addr, path: String): Unit =
    log(s"handleCall: $addr, $path")
    if (access(addr, Str("Call")) == Absent) {
      app :> s"$$assert.notCallable($path);"
    } else app :> s"$$assert.callable($path);"

  // handle [[Construct]]
  private def handleConstruct(addr: Addr, path: String): Unit =
    log(s"handleConstruct: $addr, $path")
    if (access(addr, Str("Construct")) == Absent) {
      app :> s"$$assert.notConstructable($path);"
    } else app :> s"$$assert.constructable($path);"

  // handle property names
  private def handlePropKeys(addr: Addr, path: String): Unit =
    log(s"handlePropKeys: $addr, $path")
    val newSt = exitSt.copied
    getValue(addr, "OwnPropertyKeys") match
      case Clo(f, _) =>
        newSt.context = Context(f, MMap(Name("O") -> addr))
        newSt.callStack = Nil
        Interpreter(newSt)
        val propsAddr = newSt(GLOBAL_RESULT) match
          case Comp(_, addr: Addr, _) => addr
          case addr: Addr if (newSt(addr).isCompletion) => (
            newSt(addr)(Str("Value")) match
              case valueAddr: Addr => valueAddr
              // TODO : Is this next line correct? should we throw error?
              case _ => addr
            // error(s"not an address: $v")
          )
          case addr: Addr => addr
          case v          => error(s"not an address: $v")
        val len = newSt(propsAddr, Str("length")).asMath.toInt
        val array = (0 until len)
          .map(k => newSt(propsAddr, Math(k)))
          .flatMap(_ match {
            case Str(str)   => Some(s"'$str'")
            case addr: Addr => addrToName(addr)
            case _          => None
          })
        if (array.length == len)
          app :> s"$$assert.compareArray(Reflect.ownKeys($path), ${array
            .mkString("[", ", ", "]")}, $path);"
      case _ => warning("non-closure [[OwnPropertyKeys]]: $path")

  // handle properties
  private lazy val fields =
    List("Get", "Set", "Value", "Writable", "Enumerable", "Configurable")
  private def handleProperty(addr: Addr, path: String): Unit =
    log(s"handleProperty: $addr, $path")
    val map = access(addr, Str(INNER_MAP))
    for (p <- getKeys(map, path)) access(map, p) match
      case addr: Addr =>
        exitSt(addr) match
          // NOTE : next line cannot be MapObj
          case RecordObj(
                "PropertyDescriptor" | "DataProperty" | "AccessorProperty",
                props,
              ) =>
            var set = Set[String]()
            val2str(p).map(propStr => {
              for {
                field <- fields
                value <- props.get(field)
              } value match
                case sv: SimpleValue =>
                  set += s"${field.toLowerCase}: ${sv2str(sv)}"
                case addr: Addr =>
                  field match
                    case "Value" => handleObject(addr, s"$path[$propStr]")
                    case "Get" =>
                      handleObject(
                        addr,
                        s"Object.getOwnPropertyDescriptor($path, $propStr).get",
                      )
                    case "Set" =>
                      handleObject(
                        addr,
                        s"Object.getOwnPropertyDescriptor($path, $propStr).set",
                      )
                    case _ =>
                case _ => warning("invalid property: $path")
              val desc = set.mkString("{ ", ", ", "}")
              app :> s"$$verifyProperty($path, $propStr, $desc);"
            })
          case x => warning("invalid property: $path")
      case v => warning("invalid property: $path")

  // get values
  private def getValue(str: String): Value = getValue(Expr.from(str))
  private def getValue(expr: Expr): Value =
    (new Interpreter(exitSt.copied)).eval(expr)
  private def getValue(rt: RefTarget): Value = exitSt(rt)
  private def getValue(addr: Addr, prop: String): Value =
    getValue(FieldTarget(addr, Str(prop)))

  // access properties
  private def access(base: Value, props: Value*): Value =
    props.foldLeft(base) { case (base, p) => exitSt(base, p) }

  // get created lexical variables
  private lazy val lexRecord = s"@REALM.GlobalEnv.DeclarativeRecord.$INNER_MAP"
  private lazy val createdLets: Set[String] =
    getStrKeys(getValue(lexRecord), "<global-decl-record>")

  // get keys
  private def getStrKeys(value: Value, path: String): Set[String] =
    getKeys(value, path).collect { case Str(p) => p }
  private def getKeys(value: Value, path: String): Set[Value] = value match
    case addr: Addr =>
      exitSt(addr) match
        case m: MapObj => m.map.keySet.toSet
        case _ => warning(s"[[$INNER_MAP]] is not a map object: $path"); Set()
    case _ => warning(s"[[$INNER_MAP]] is not an address: $path"); Set()

  // conversion to ECMAScript code
  private def val2str(value: Value): Option[String] = value match
    case sv: SimpleValue => Some(sv.toString)
    case addr: Addr      => addrToName(addr)
    case x               => None
  private def sv2str(sv: SimpleValue): String = sv match
    case Number(n) => n.toString
    case v         => v.toString
}
