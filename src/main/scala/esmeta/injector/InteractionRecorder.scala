package esmeta.injector

import esmeta.cfg.{Call, Func}
import esmeta.es.builtin.INNER_MAP
import esmeta.ir.{GLOBAL_REALM, Local}
import esmeta.state.*
import esmeta.util.BaseUtils.raise
import scala.collection.mutable.{Map => MMap, ListBuffer}

/** records proxy internal method calls without executing logging traps */
class InteractionRecorder(
  initSt: State,
  observers: List[String],
  timeLimit: Option[Int],
) extends ExitStateExtractor(initSt, timeLimit) {
  private val proxyIds = MMap[Addr, (String, Int)]()
  private val traces = observers.map(_ -> ListBuffer[String]()).toMap

  def logs: Map[String, Vector[String]] =
    traces.map((name, entries) => name -> entries.toVector)

  private val functionCall =
    initSt.cfg.fnameMap("Record[ECMAScriptFunctionObject].Call")
  private val proxyMethods = InteractionRecorder.traps.map { (method, trap) =>
    val func = initSt.cfg.fnameMap(s"Record[ProxyExoticObject].$method")
    func.id -> trap
  }

  override protected def createContext(
    call: Call,
    func: Func,
    locals: MMap[Local, Value],
    prevCtxt: Option[Context],
  ): Context = {
    val context = super.createContext(call, func, locals, prevCtxt)
    for {
      trap <- proxyMethods.get(func.id)
      case proxy: Addr <- locals.get(func.irFunc.params.head.lhs)
      (name, id) <- proxyIds.get(proxy)
    } {
      val key =
        if (InteractionRecorder.propertyTraps(trap))
          propertyKey(locals(func.irFunc.params(1).lhs))
        else ""
      traces(name) += s"$id:$trap$key"
    }
    context
  }

  override def eval(cursor: Cursor): Boolean = {
    cursor match {
      case ExitCursor(func) if func == functionCall =>
        for {
          name <- observers
          realm <- st.globals.get(GLOBAL_REALM)
          global <- st.get(realm, Str("GlobalObject")).toOption
          logger <- property(global, s"L${name.stripPrefix("logState")}")
          if st.locals.get(func.irFunc.params.head.lhs).contains(logger)
          (_, result) <- st.context.retVal
          case proxy: Addr <- normalValue(result)
          handler <- st.get(proxy, Str("ProxyHandler")).toOption
          case Number(id) <- property(handler, "id")
          if id.isWhole && id > 0 && id <= Int.MaxValue
        } proxyIds(proxy) = name -> id.toInt
      case _ =>
    }
    super.eval(cursor)
  }

  private def property(obj: Value, key: String): Option[Value] = for {
    map <- st.get(obj, Str(INNER_MAP)).toOption
    desc <- st.get(map, Str(key)).toOption
    value <- st.get(desc, Str("Value")).toOption
  } yield value

  private def normalValue(value: Value): Option[Value] = value match {
    case addr: Addr =>
      st(addr) match {
        case RecordObj("CompletionRecord", fields) =>
          fields
            .get("Value")
            .filter(_ => fields.get("Type").contains(Enum("normal")))
        case _ => Some(value)
      }
    case _ => Some(value)
  }

  private def propertyKey(value: Value): String = value match {
    case Str(key) => s":string:$key"
    case addr: Addr =>
      val description = st(addr) match {
        case RecordObj("Symbol", fields) =>
          fields.get("Description") match
            case Some(Str(text)) => text
            case Some(Undef)     => ""
            case _ => raise("invalid symbol description in interaction log")
        case _ => raise("invalid property key in interaction log")
      }
      s":symbol:Symbol($description)"
    case _ => raise("invalid property key in interaction log")
  }
}

object InteractionRecorder {
  private val propertyTraps = Set(
    "get",
    "set",
    "has",
    "deleteProperty",
    "defineProperty",
    "getOwnPropertyDescriptor",
  )
  private val traps = Map(
    "GetPrototypeOf" -> "getPrototypeOf",
    "SetPrototypeOf" -> "setPrototypeOf",
    "IsExtensible" -> "isExtensible",
    "PreventExtensions" -> "preventExtensions",
    "GetOwnProperty" -> "getOwnPropertyDescriptor",
    "DefineOwnProperty" -> "defineProperty",
    "HasProperty" -> "has",
    "Get" -> "get",
    "Set" -> "set",
    "Delete" -> "deleteProperty",
    "OwnPropertyKeys" -> "ownKeys",
    "Call" -> "apply",
    "Construct" -> "construct",
  )
}
