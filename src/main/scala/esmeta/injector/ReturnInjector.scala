package esmeta.injector

import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.ir.*
import esmeta.interpreter.Interpreter
import esmeta.es.ESElem
import esmeta.es.builtin.INNER_MAP
import scala.collection.mutable.ListBuffer

class ReturnInjector(cfg: CFG, exitSt: State):
  /** generated assertions */
  lazy val assertions: Vector[ReturnAssertion] =
    _returns.clear
    if (normalExit)
      handleVariable
      handleLet
    _returns.toVector

  def getLets = createdLets
  def getVars = createdVars

  /** exit status tag */
  lazy val exitTag: ExitTag = ExitTag(exitSt)

  /** normal termination */
  lazy val normalExit: Boolean = exitTag == NormalTag

  private def handleObject(addr: Addr, path: String): Unit =
    println(s"handleObject: $addr, $path")
    (addr, handledObjects.get(addr)) match
      case (_, Some(origPath)) =>
        _returns += ReturnVariable(path)
      case (_: DynamicAddr, None) if addr != globalThis =>
        handledObjects += addr -> path
        exitSt(addr) match
          case (_: MapObj) =>
            handleProperty(addr, path)
          case _ =>
      case _ =>

  // handle variables
  private def handleVariable: Unit = for (x <- createdVars.toList.sorted) {
    println("handling variable...")
    val path = s"globalThis[\"$x\"]"
    getValue(s"""$globalMap["$x"].Value""") match
      case Absent          => /* handle global accessor property */
      case sv: SimpleValue => _returns += ReturnVariable(x)
      case addr: Addr      => handleObject(addr, x)
      case _               =>
  }

  // handle lexical variables
  private def handleLet: Unit = for (x <- createdLets.toList.sorted) {
    println("handling let...")
    getValue(s"""$lexRecord["$x"].BoundValue""") match
      case sv: SimpleValue => _returns += ReturnVariable(x)
      case addr: Addr      => handleObject(addr, x)
      case _               =>
  }

  // handle properties
  private lazy val fields =
    List("Get", "Set", "Value", "Writable", "Enumerable", "Configurable")
  private def handleProperty(addr: Addr, path: String): Unit =
    println(s"handleProperty: $addr, $path")
    val map = access(addr, Str(INNER_MAP))
    for (p <- getKeys(map, path)) access(map, p) match
      case addr: Addr =>
        exitSt(addr) match
          case RecordObj(
                "PropertyDescriptor" | "DataProperty" | "AccessorProperty",
                props,
              ) =>
            var desc: Map[String, SimpleValue] = Map.empty
            val2str(p).map(propStr => {
              for {
                field <- fields
                value <- props.get(field)
              } value match
                case sv: SimpleValue =>
                  desc += (field.toLowerCase -> sv)
                case addr: Addr =>
                  field match
                    case "Value" => handleObject(addr, s"$path[$propStr]")
                    case "Get"   =>
                    case "Set"   =>
                    case _       =>
                case _ => println("invalid property: $path")
              _returns += ReturnVariable(s"$path[$propStr]")
            })
          case x => println("invalid property: $path")
      case v => println("invalid property: $path")

  private val _returns: ListBuffer[ReturnAssertion] = ListBuffer()

  private lazy val initHeap = cfg.init.initHeap.copied
  private var handledObjects: Map[Addr, String] = (for {
    addr <- initHeap.map.keySet
    name <- addrToName(addr)
  } yield addr -> name).toMap
  private lazy val PREFIX_INTRINSIC = "INTRINSICS."
  private def addrToName(addr: Addr): Option[String] = addr match
    case a @ NamedAddr(name) if name.startsWith(PREFIX_INTRINSIC) =>
      Some(name.substring(PREFIX_INTRINSIC.length))
    case _ => None

  // get created variables
  private lazy val globalMap = s"@REALM.GlobalObject.$INNER_MAP"
  private lazy val globalThis =
    getValue(s"$globalMap.globalThis.Value")
  private lazy val createdVars: Set[String] =
    val initial = getStrKeys(getValue(s"@GLOBAL.$INNER_MAP"), "<global>")
    val current = getStrKeys(getValue(globalMap), "<global>")
    current -- initial

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
        case _ => println(s"[[$INNER_MAP]] is not a map object: $path"); Set()
    case _ => println(s"[[$INNER_MAP]] is not an address: $path"); Set()

  // conversion to ECMAScript code
  private def val2str(value: Value): Option[String] = value match
    case sv: SimpleValue => Some(sv.toString)
    case addr: Addr      => addrToName(addr)
    case x               => None

trait ReturnAssertion extends ESElem

case class ReturnVariable(x: String) extends ReturnAssertion
