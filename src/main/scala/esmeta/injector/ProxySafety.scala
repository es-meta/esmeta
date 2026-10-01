package esmeta.injector

import esmeta.cfg.{Call, Func}
import esmeta.es.builtin.INNER_MAP
import esmeta.interpreter.Interpreter
import esmeta.ir.{ETypeCheck, Expr, GLOBAL_REALM, Local, Ref}
import esmeta.state.*
import esmeta.ty.ValueTy
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** finds the value sites whose objects a trap-free proxy cannot replace
  *
  * The program wraps each candidate site `k` with `T(e, k)`, which returns `e`
  * as is. During one run of the original semantics, every object that reaches a
  * site is tagged with it. A site is unsafe if some tagged object is used in a
  * way a proxy of it would not reproduce:
  *
  *   - a field the proxy lacks (an internal slot, [[Prototype]], the property
  *     map, ...) is accessed outside the object's own internal methods, or
  *   - a type check gives another answer for a proxy of the object.
  *
  * Inside its own internal methods the object is the target the proxy forwards
  * to, so accesses there are kept by the proxy. The result is a guess; the
  * instrumented run on the specification decides.
  */
class ProxySafety(initSt: State, tagger: String, timeLimit: Option[Int])
  extends Interpreter(initSt, timeLimit = timeLimit) {

  /** sites whose values reached each object */
  val tags: MMap[Addr, MSet[Int]] = MMap()

  /** sites some of whose objects a proxy cannot replace */
  val unsafe: MSet[Int] = MSet()

  /** why each site is unsafe: the access and the function it happened in */
  val reasons: MMap[Int, MSet[String]] = MMap()

  /** sites that produced an object at least once */
  def objectSites: Set[Int] = tags.values.flatten.toSet

  private val functionCall =
    initSt.cfg.fnameMap("Record[ECMAScriptFunctionObject].Call")

  /** contexts of internal methods, with the object they run on */
  private val selfOf = java.util.IdentityHashMap[Context, Addr]()

  override protected def createContext(
    call: Call,
    func: Func,
    locals: MMap[Local, Value],
    prevCtxt: Option[Context],
  ): Context = {
    val context = super.createContext(call, func, locals, prevCtxt)
    func.irFunc.name match
      case ProxySafety.MethodName(method) if ProxySafety.methods(method) =>
        func.irFunc.params.headOption.flatMap(p => locals.get(p.lhs)) match
          case Some(addr: Addr) if tags.contains(addr) =>
            selfOf.put(context, addr)
          case _ =>
      case _ =>
    context
  }

  /** whether a tagged object runs one of its own internal methods */
  private def inSelf(addr: Addr): Boolean =
    (st.context :: st.callStack.map(_.context)).exists(c =>
      selfOf.get(c) eq addr,
    )

  private def flag(addr: Addr, why: => String): Unit =
    if (!inSelf(addr)) {
      unsafe ++= tags(addr)
      for (site <- tags(addr))
        reasons.getOrElseUpdate(
          site,
          MSet(),
        ) += s"$why in ${st.func.irFunc.name}"
    }

  override def eval(ref: Ref): RefTarget = {
    val target = super.eval(ref)
    target match
      case FieldTarget(addr: Addr, Str(field))
          if tags.contains(addr) && !ProxySafety.proxyFields(field) =>
        flag(addr, s"field $field")
      case _ =>
    target
  }

  override def eval(expr: Expr): Value = expr match
    case ETypeCheck(base, ty) =>
      val value = eval(base)
      val result = ty.ty.contains(value, st)
      value match
        case addr: Addr if tags.contains(addr) =>
          (st(addr), ty.ty) match
            case (obj: RecordObj, vty: ValueTy)
                if vty.record.contains(proxyOf(addr, obj), st.heap) != result =>
              flag(addr, s"type check ${ty.ty}")
            case _ =>
        case _ =>
      Bool(result)
    case _ => super.eval(expr)

  /** a stand-in record of a trap-free proxy of an object */
  private def proxyOf(addr: Addr, obj: RecordObj): RecordObj =
    val map = MMap[String, Value]("ProxyTarget" -> addr, "ProxyHandler" -> addr)
    for (m <- List("Call", "Construct"); v <- obj.map.get(m)) map += m -> v
    RecordObj("ProxyExoticObject", map)

  /** tag the object passed to the tagging function with its site */
  override def eval(cursor: Cursor): Boolean = {
    cursor match {
      case ExitCursor(func) if func == functionCall =>
        for {
          realm <- st.globals.get(GLOBAL_REALM)
          global <- st.get(realm, Str("GlobalObject")).toOption
          fn <- property(global, tagger)
          if func.irFunc.params.headOption
            .flatMap(p => st.locals.get(p.lhs))
            .contains(fn)
          args <- func.irFunc.params.lift(2).flatMap(p => st.locals.get(p.lhs))
          case list: Addr <- Some(args)
          case ListObj(values) <- Some(st(list))
          case (value: Addr) +: Number(site) +: _ <- Some(values.toVector)
          if site.isWhole
        } tags.getOrElseUpdate(value, MSet()) += site.toInt
      case _ =>
    }
    super.eval(cursor)
  }

  private def property(obj: Value, key: String): Option[Value] = for {
    map <- st.get(obj, Str(INNER_MAP)).toOption
    desc <- st.get(map, Str(key)).toOption
    value <- st.get(desc, Str("Value")).toOption
  } yield value
}

object ProxySafety {
  val MethodName = """Record\[[^\]]+\]\.(\w+)""".r

  /** the internal methods, which a proxy also has */
  val methods = Set(
    "GetPrototypeOf",
    "SetPrototypeOf",
    "IsExtensible",
    "PreventExtensions",
    "GetOwnProperty",
    "DefineOwnProperty",
    "HasProperty",
    "Get",
    "Set",
    "Delete",
    "OwnPropertyKeys",
    "Call",
    "Construct",
  )

  /** fields a trap-free proxy answers like its target */
  val proxyFields = methods
}
