package esmeta.js.util

import esmeta.js.*
import esmeta.ir.*
import esmeta.interp.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{LINE_SEP, RESOURCE_DIR}
import scala.concurrent.TimeoutException
import scala.collection.mutable.{Map => MMap}

class Injector(
  st: State,
  timeLimit: Option[Long],
  log: Boolean = false,
) {
  // injected script
  lazy val result: String = {
    log(scriptStr)
    injectTag(interpResult) // run interpreter and append tag
    append(scriptStr) // append script
    if (isNormal) {
      if (isAsync) startAsync // handle async
      handleVariable // inject assertions from variables
      handleLet // inject assertions from lexical variables
      if (isAsync) endAsync
    }
    getString
  }

  // string
  // XXX use pared ast?
  private lazy val scriptStr = st.sourceText.get

  // logging
  private def log(any: Any): Unit = if (log) println(any)
  private def warning: Unit = if (log) {
    val trace = (new Throwable).getStackTrace
    val line = trace(1).getLineNumber
    println(s"[Warning] $scriptStr @ $line")
  }

  // run interp
  private var addrNameMap: Map[Addr, String] = Map()
  private val interp = new Interp(st, Nil, false) {
    // hook return for addr name map
    override def setReturn(value: Value): Unit = {
      super.setReturn(value)
      if (this.st.context.name == "MakeBasicObject") {
        val contexts = (this.st.context :: this.st.callStack.map(_.context))
          .filter(c => c.func.isSDO || c.func.isBuiltin)
        (value, contexts) match
          case (addr: Addr, ctxt :: _) => addrNameMap += (addr -> ctxt.name)
          case _                       => /* do nothing */
      }
    }
  }
  private var handledObjects: Map[Addr, String] = (for {
    addr <- st.heap.map.keySet
    name <- addrToName(addr)
  } yield addr -> name).toMap
  private lazy val interpResult: Either[Throwable, Value] =
    try {
      timeout(interp.fixpoint, timeLimit)
      Right(st(GLOBAL_RESULT))
    } catch { case e => Left(e) }
  private lazy val isNormal: Boolean = interpResult match
    case Right(_: PureValue) => true
    case _                   => false

  // --------------------------------------------------------------------------
  // Helper Functions
  // --------------------------------------------------------------------------
  // add line
  private var sb = new StringBuilder
  private def append(str: String, comment: Boolean = false): Unit = {
    if (comment) sb.append("// ")
    sb.append(str).append(LINE_SEP)
  }
  private def getString: String = sb.toString

  // handle variables
  private def handleVariable: Unit = for (x <- createdVars.toList.sorted) {
    log("handling varaible...")
    getValue(s"""$globalMap["$x"].Value""") match
      case Absent => /* do nothing(handle global accessor property) */
      case sv: SimpleValue =>
        append(s"$$assert.sameValue($x, ${sv2str(sv)});")
      case addr: Addr => handleObject(addr, x)
      case _          => /* do nothing */
  }

  // handle lexical variables
  private def handleLet: Unit = for (x <- createdLets.toList.sorted) {
    log("handling let...")
    getValue(s"""$lexRecord["$x"].BoundValue""") match
      case sv: SimpleValue => append(s"$$assert.sameValue($x, ${sv2str(sv)});")
      case addr: Addr      => handleObject(addr, x)
      case _               => /* do nothing */
  }

  // handle addresses
  private lazy val PREFIX_INTRINSIC = "INTRINSICS."
  private def addrToName(addr: Addr): Option[String] = addr match {
    case a @ NamedAddr(name) if name.startsWith(PREFIX_INTRINSIC) =>
      Some(name.substring(PREFIX_INTRINSIC.length))
    case _ => None
  }
  private def handleObject(addr: Addr, path: String): Unit = {
    log(s"handleObject: $addr, $path")
    (addr, handledObjects.get(addr)) match {
      case (_, Some(origPath)) =>
        append(s"$$assert.sameValue($path, $origPath);")
      case (_: DynamicAddr, None) if addr != globalThis =>
        handledObjects += addr -> path
        addrNameMap
          .get(addr)
          .map(name => append(s"""$$algo.set($path, "$name")"""))
        st(addr) match {
          case (_: MapObj) =>
            handlePrototype(addr, path)
            handleExtensible(addr, path)
            handleCall(addr, path)
            handleConstruct(addr, path)
            handlePropKeys(addr, path)
            handleProperty(addr, path)
          case _ =>
        }
      case _ =>
    }
  }

  // handle [[Prototype]]
  private def handlePrototype(addr: Addr, path: String): Unit = {
    log(s"handlePrototype: $addr, $path")
    access(addr, Str("Prototype")) match
      case addr: Addr => handleObject(addr, s"Object.getPrototypeOf($path)")
      case _          => warning
  }

  // handle [[Extensible]]
  private def handleExtensible(addr: Addr, path: String): Unit = {
    log(s"handleExtensible: $addr, $path")
    access(addr, Str("Extensible")) match
      case Bool(b) =>
        append(s"$$assert.sameValue(Object.isExtensible($path), $b);")
      case _ => warning
  }

  // handle [[Call]]
  private def handleCall(addr: Addr, path: String): Unit = {
    log(s"handleCall: $addr, $path")
    if (access(addr, Str("Call")) == Absent) {
      append(s"$$assert.notCallable($path);")
    } else append(s"$$assert.callable($path);")
  }

  // handle [[Construct]]
  private def handleConstruct(addr: Addr, path: String): Unit = {
    log(s"handleConstruct: $addr, $path")
    if (access(addr, Str("Construct")) == Absent) {
      append(s"$$assert.notConstructable($path);")
    } else append(s"$$assert.constructable($path);")
  }

  // handle property names
  private def handlePropKeys(addr: Addr, path: String): Unit = {
    log(s"handlePropKeys: $addr, $path")
    val newSt = st.copied
    getValue(addr, "OwnPropertyKeys") match
      case Clo(f, _) =>
        newSt.context = Context(f, MMap(Name("O") -> addr))
        newSt.callStack = Nil
        Interp(newSt)
        val propsAddr = newSt(GLOBAL_RESULT) match
          case Comp(_, addr: Addr, _) => addr
          case addr: Addr             => addr
          case v                      => error("not an address: $v")
        val len = newSt(propsAddr, Str("length")).asMath.toInt
        val array = (0 until len)
          .map(k => newSt(propsAddr, Math(k)))
          .flatMap(_ match {
            case Str(str)   => Some(s"'$str'")
            case addr: Addr => addrToName(addr)
            case _          => None
          })
        if (array.length == len)
          append(s"$$assert.compareArray(Reflect.ownKeys($path), ${array
            .mkString("[", ", ", "]")}, $path);")
      case _ => warning

  }

  // handle properties
  private lazy val fields =
    List("Get", "Set", "Value", "Writable", "Enumerable", "Configurable")
  private def handleProperty(addr: Addr, path: String): Unit = {
    log(s"handleProperty: $addr, $path")
    val subMap = access(addr, Str("SubMap"))
    for (p <- getKeys(subMap)) access(subMap, p) match {
      case addr: Addr =>
        st(addr) match {
          case MapObj(
                "PropertyDescriptor" | "DataProperty" | "AccessorProperty",
                props,
                _,
              ) =>
            var set = Set[String]()
            val2str(p).map(propStr => {
              for {
                field <- fields
                value <- props.get(Str(field)).map(_.value.toPureValue)
              } value match {
                case sv: SimpleValue =>
                  set += s"${field.toLowerCase}: ${sv2str(sv)}"
                case addr: Addr =>
                  field match {
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
                  }
                case _ => warning
              }
              val desc = set.mkString("{ ", ", ", "}")
              append(s"$$verifyProperty($path, $propStr, $desc);")
            })
          case x => warning
        }
      case v => warning
    }
  }

  // handle async
  lazy val isAsync: Boolean = {
    scriptStr.contains("async") || scriptStr.contains("Promise")
  }
  private def startAsync: Unit = {
    log("handling async...")
    append("$delay(() => {")
  }
  private def endAsync: Unit = {
    append("});")
  }

  // inject tag
  private lazy val errorNameRegex = "INTRINSICS.([A-Z][a-z]+)Error.prototype".r
  private def injectTag(result: Either[Throwable, Value]): Unit = {
    log("injecting tag...")
    val tag = result match
      case Right(Undef) => "Normal:"
      // chekc error object
      case Right(comp @ Comp(CONST_THROW, addr: DynamicAddr, _)) =>
        getValue(addr, "Prototype") match
          case NamedAddr(errorNameRegex(errorName)) => s"${errorName}Error:"
          case _ => s"Throw $comp" // XXX possible?
      case Right(v)                  => s"Throw $v:" // XXX possible?
      case Left(_: TimeoutException) => "Timeout:"
      case Left(_)                   => "ESMetaError:"
    append(tag, comment = true)
  }

  // get values
  def getValue(str: String): Value = getValue(Expr.from(str))
  def getValue(expr: Expr): Value =
    (new Interp(st.copied, Nil, false)).interp(expr)
  def getValue(refV: RefValue): Value = st(refV)
  def getValue(addr: Addr, prop: String): Value =
    getValue(PropValue(addr, Str(prop)))

  // access properties
  private def access(base: Value, props: PureValue*): Value =
    props.foldLeft(base) { case (base, p) => st(base, p) }

  // get created variables
  private lazy val globalMap = "@REALM.GlobalObject.SubMap"
  private lazy val globalThis =
    getValue(s"$globalMap.globalThis.Value")
  private lazy val createdVars: Set[String] = {
    val initial = getStrKeys(getValue("@GLOBAL.SubMap"))
    val current = getStrKeys(getValue(globalMap))
    current -- initial
  }

  // get created lexical variables
  private lazy val lexRecord =
    "@REALM.GlobalEnv.DeclarativeRecord.SubMap"
  private lazy val createdLets: Set[String] =
    getStrKeys(getValue(lexRecord))

  // get keys
  private def getStrKeys(value: Value): Set[String] =
    getKeys(value).collect { case Str(p) => p }
  private def getKeys(value: Value): Set[PureValue] = value match
    case addr: Addr =>
      st(addr) match
        case m: MapObj => m.props.keySet.toSet
        case _         => warning; Set()
    case _ => warning; Set()

  // conversion to JS codes
  private def val2str(value: Value): Option[String] = value match
    case sv: SimpleValue => Some(sv.toString)
    case addr: Addr      => addrToName(addr)
    case x               => None
  private def sv2str(sv: SimpleValue): String = sv match
    case Number(n) => n.toString
    case v         => v.toString
}

object Injector {

  /** run an interpreter and inject code for checking final states */
  def apply(
    st: State,
    defs: Boolean = false,
    timeLimit: Option[Long] = Some(TIMEOUT),
    log: Boolean = false,
  ): String = {
    val injected = new Injector(st, timeLimit, log).result
    if (defs) readFile(s"$RESOURCE_DIR/assertions.js") + LINE_SEP + injected
    else injected
  }
}
