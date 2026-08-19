package esmeta.solver

import esmeta.cfg.Func
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

trait Solver { self: SymInterp =>

  import tychecker.*, SymTy.*, Solver.*

  /** check the satisfiability of the given abstract state */
  def check: Boolean =
    val AbsState(reachable, locals, symEnv, constr) = st
    reachable &&
    symEnv.forall { case (sym, ty) => !ty.isBottom }

  /** reify a satisfiable path into an ECMAScript program */
  def reify: Option[String] = reifyAll.headOption

  def reifyAll: LazyList[String] =
    given AbsState = st
    val thisValue = st.getConstr(SThis.sym)
    val newTarget = st.getConstr(SNewTarget.sym)
    val args = entryFunc.head match
      case Some(h: BuiltinHead) =>
        val variadicAt = h.params.indexWhere(_.kind == ParamKind.Variadic)
        if (variadicAt < 0) // no variadic argument
          (0 until h.arity._2).toList.map(i => st.getConstr(i))
        else // contains variadic argument
          val fixed = h.params.indices
            .filter(_ != variadicAt)
            .map(i => st.getConstr(i))
            .toList
          val (before, after) = fixed.splitAt(variadicAt)
          // only a refined argument is in the environment
          st.symEnv.keysIterator.flatMap(variadicIdxOf).maxOption match
            case None => before ++ after
            case Some(i) =>
              val variadic = (0 to i).toList.map { k =>
                st.getConstr(SVariadicIdx(k).sym)
              }
              before ++ variadic ++ after
      case _ => Nil
    getPath(entryFunc) match
      case None => LazyList.empty
      case Some(path) =>
        val thisCands = candidates(thisValue).toList
        val argCands = args.map(candidates(_).toList)
        val ntCands = newTargetCandidates(newTarget)
        val slots = (thisCands :: argCands) :+ ntCands
        oneChange(slots).flatMap {
          case thisV :: rest =>
            rest.splitAt(args.length) match
              case (vs, nt :: Nil) =>
                buildJSProgram(path, thisV, vs, Option.when(nt.nonEmpty)(nt))
              case _ => None
          case _ => None
        }
}
object Solver {

  // ---------------------------------------------------------------------------
  // candidate enumeration
  // ---------------------------------------------------------------------------
  def candidates(ty: ValueTy): LazyList[String] = distinct(
    exact(ty).to(LazyList) #::: fromShape(ty) #::: witnessesFor(ty).to(LazyList),
  )

  def exprFor(ty: ValueTy): Option[String] = candidates(ty).headOption

  private def exact(ty: ValueTy): List[String] =
    def one[T](flat: Flat[T])(f: T => String): List[String] =
      flat match
        case One(x) => List(f(x))
        case _      => Nil
    val numbers = ty.number.toNumberSet.fold(Nil) { set =>
      set.toList.sortBy(n => (n.isNaN, n.double)).map(numberLit)
    }
    numbers ++
    one(ty.bigInt)(n => s"${n}n") ++
    one(ty.str)(str => s"\"${normStr(str)}\"") ++
    one(ty.bool.getSingle)(b => if (b) "true" else "false")

  private def numberLit(n: Number): String =
    val d = n.double
    if (d.isNaN) "NaN"
    else if (d.isPosInfinity) "Infinity"
    else if (d.isNegInfinity) "-Infinity"
    else if (d == 0 && 1 / d < 0) "-0"
    else if (d.isWhole && d.abs <= 9007199254740991.0) d.toLong.toString
    else d.toString

  def newTargetCandidates(ty: ValueTy): List[String] =
    if (UndefT ⊑ ty) List("")
    else candidates(ty).toList

  def oneChange(slots: List[List[String]]): LazyList[List[String]] =
    if (slots.exists(_.isEmpty)) LazyList.empty
    else
      val heads = slots.map(_.head)
      val tails = slots.map(_.tail.toVector)
      val rounds = tails.map(_.size).maxOption.getOrElse(0)
      val variants = for {
        k <- (0 until rounds).iterator
        (alts, i) <- tails.iterator.zipWithIndex
        if (k < alts.size)
      } yield heads.updated(i, alts(k))
      heads #:: variants.to(LazyList)

  private def distinct(xs: LazyList[String]): LazyList[String] =
    val seen = scala.collection.mutable.Set[String]()
    xs.filter(seen.add)

  // the loose bound decides what applies, the strict one decides what comes first
  private def witnessesFor(ty: ValueTy): List[String] =
    if (ty.isBottom) Nil
    else
      val strict = unrefined(ty)
      val loose = erased(ty)
      cachedWitnesses.getOrElseUpdate(
        (strict, loose), {
          val applies = witnesses.filter { case (wty, _) => wty <= loose }
          val (fit, rest) = applies.partition { case (wty, _) => wty <= strict }
          (roundRobin(fit.map(_._2)) ++ roundRobin(rest.map(_._2))).distinct
        },
      )

  // the same bound recurs across every path of every entry
  private val cachedWitnesses =
    collection.concurrent.TrieMap[(ValueTy, ValueTy), List[String]]()

  // one from each row in turn, so a long row cannot take the whole budget
  private def roundRobin(rows: List[List[String]]): List[String] =
    val cols = rows.map(_.toVector)
    val rounds = cols.map(_.size).maxOption.getOrElse(0)
    (0 until rounds).toList.flatMap { i =>
      cols.collect { case col if i < col.size => col(i) }
    }

  private def unrefined(ty: ValueTy): ValueTy = ty.record match
    case RecordTy.Elem(map, obj) =>
      ty.copied(record = RecordTy.Elem(map.map((t, fm) => t -> widen(fm)), obj))
    case _ => ty

  private def erased(ty: ValueTy): ValueTy = ty.record match
    case RecordTy.Elem(map, obj) =>
      ty.copied(record =
        RecordTy.Elem(map.map((t, _) => t -> FieldMap.Top), obj),
      )
    case _ => ty

  // keep whether a field is there, drop what its value is
  private def widen(fm: FieldMap): FieldMap = FieldMap(fm.map.collect {
    case (field, binding) if !binding.absent => field -> Binding(AnyT)
  })

  private val typedArrayEntries: List[(ValueTy, List[String])] =
    val names = List(
      "Int8Array",
      "Uint8Array",
      "Uint8ClampedArray",
      "Int16Array",
      "Uint16Array",
      "Int32Array",
      "Uint32Array",
      "BigInt64Array",
      "BigUint64Array",
      "Float16Array",
      "Float32Array",
      "Float64Array",
    )
    for (name <- names) yield RecordT(name) -> List(s"new $name()")

  private def buildJSProgram(
    path: BuiltinPath,
    thisV: String,
    vs: List[String],
    newTarget: Option[String],
  ): Option[String] = newTarget match
    case Some(nt) =>
      access(path).map(target =>
        s"Reflect.construct($target, [${vs.mkString(", ")}], $nt);",
      )
    case None =>
      path match
        case BuiltinPath.YetPath(_) => None
        case BuiltinPath.Getter(base) =>
          descriptor(base).map(d => s"$d.get.call($thisV);")
        case BuiltinPath.Setter(base) =>
          val value = vs.headOption.getOrElse("undefined")
          descriptor(base).map(d => s"$d.set.call($thisV, $value);")
        case _ =>
          val args = (thisV :: vs).mkString(", ")
          access(path).map(fn => s"$fn.call($args);")

  def getPath(func: Func): Option[BuiltinPath] = func.head match {
    case Some(h: BuiltinHead) => Some(h.path)
    case _                    => None
  }

  // JS expression to access a builtin function (None if unreachable)
  def funcAccessExpr(f: Func): Option[String] =
    f.head.collectFirst { case h: BuiltinHead => h.path }.flatMap(access)

  // JS expression accessing the builtin at path
  private def access(path: BuiltinPath): Option[String] = path match
    case BuiltinPath.Base(name) =>
      globalAlias.get(name) match
        case Some("")   => None // intrinsic unreachable from JS
        case Some(expr) => Some(expr)
        case None       => Some(name) // directly nameable global
    case BuiltinPath.NormalAccess(base, name) =>
      access(base).map(b => s"$b.$name")
    case BuiltinPath.SymbolAccess(base, sym) =>
      access(base).map(b => s"$b[Symbol.$sym]")
    case BuiltinPath.Getter(base) => access(base)
    case BuiltinPath.Setter(base) => access(base)
    case BuiltinPath.YetPath(_)   => None

  // Object.getOwnPropertyDescriptor(target, key) for a getter/setter base
  private def descriptor(base: BuiltinPath): Option[String] = base match
    case BuiltinPath.NormalAccess(b, n) =>
      val target = access(b)
      val key = s"\"${normStr(n)}\""
      target.map(t => s"Object.getOwnPropertyDescriptor($t, $key)")
    case BuiltinPath.SymbolAccess(b, s) =>
      val target = access(b)
      val key = s"Symbol.$s"
      target.map(t => s"Object.getOwnPropertyDescriptor($t, $key)")
    case _ => None

  // global alias for builtins that are not directly nameable but have a known JS expression to access them
  // https://github.com/tc39/test262/blob/main/harness/wellKnownIntrinsicObjects.js
  private val globalAlias: Map[String, String] = Map(
    "TypedArray" -> "Object.getPrototypeOf(Uint8Array)",
    "ArrayIteratorPrototype" -> "Object.getPrototypeOf([][Symbol.iterator]())",
    "AsyncFromSyncIteratorPrototype" -> "",
    "AsyncFunction" -> "(async function() {}).constructor",
    "AsyncGeneratorFunction" -> "(async function* () {}).constructor",
    "AsyncGeneratorPrototype" -> "Object.getPrototypeOf(async function* () {}).prototype",
    "AsyncIteratorPrototype" -> "Object.getPrototypeOf(Object.getPrototypeOf(async function* () {}).prototype)",
    "ForInIteratorPrototype" -> "",
    "GeneratorFunction" -> "(function* () {}).constructor",
    "GeneratorPrototype" -> "Object.getPrototypeOf(function * () {}).prototype",
    "IteratorHelperPrototype" -> "Object.getPrototypeOf(Iterator.from([]).drop(0))",
    "MapIteratorPrototype" -> "Object.getPrototypeOf(new Map()[Symbol.iterator]())",
    "SetIteratorPrototype" -> "Object.getPrototypeOf(new Set()[Symbol.iterator]())",
    "StringIteratorPrototype" -> "Object.getPrototypeOf(new String()[Symbol.iterator]())",
    "RegExpStringIteratorPrototype" -> """Object.getPrototypeOf(RegExp.prototype[Symbol.matchAll](""))""",
    "WrapForValidIteratorPrototype" -> "Object.getPrototypeOf(Iterator.from({ [Symbol.iterator](){ return {}; } }))",
    "ThrowTypeError" -> """(function() { "use strict"; return Object.getOwnPropertyDescriptor(arguments, "callee").get })()""",
  )

  // values built from the object shape a type carries
  private def fromShape(ty: ValueTy): LazyList[String] =
    val objs = ty.record match
      case RecordTy.Elem(map, ObjShape(props, call, construct))
          if props.nonEmpty =>
        val ordered = props.toList.sortBy { case (p, _) => propKey(p) }
        val slots = ordered.map { (prop, desc) =>
          val k = propKey(prop)
          if (desc.getExc) List(s"get $k() { throw 0; }")
          else if (desc.setExc) List(s"set $k(_) { throw 0; }")
          else candidates(desc.ty).toList.map(v => s"$k: $v")
        }
        val objs = oneChange(slots).map(_.mkString("{ ", ", ", " }"))
        if (isPlainObject(ty)) objs
        else
          val traps = ordered match
            case (prop, desc) :: Nil =>
              val key = propExpr(prop)
              val fwd = "return Reflect.get(t, p, r); }"
              if (desc.getExc)
                List(s"get(t, p, r) { if (p === $key) throw 0; $fwd")
              else if (desc.setExc)
                List(
                  s"set(t, p, v, r) { if (p === $key) throw 0; " +
                  "return Reflect.set(t, p, v, r); }",
                )
              else
                candidates(desc.ty).toList
                  .map(v => s"get(t, p, r) { if (p === $key) return $v; $fwd")
            case _ => Nil
          val base = exprFor(
            ty.copied(record =
              RecordTy.Elem(map, ObjShape(Map.empty, call, construct)),
            ),
          )
          base match
            case None => LazyList.empty
            case Some(b) =>
              traps.to(LazyList).map(h => s"new Proxy($b, { $h })") #:::
              objs.map(o =>
                s"Object.defineProperties($b, " +
                s"Object.getOwnPropertyDescriptors($o))",
              )
      case _ => LazyList.empty
    objs #::: fromConstruct(ty).iterator.to(LazyList) #:::
    fromCall(ty).iterator.to(LazyList)

  private def fromConstruct(ty: ValueTy): Option[String] =
    ty.record.construct match
      case ConstructDesc.Elem(exc, ret) =>
        if (exc) Some("function() { throw 0; }")
        else exprFor(ret).map(v => s"function() { return $v; }")
      case ConstructDesc.Top => None

  private def fromCall(ty: ValueTy): Option[String] =
    ty.record.call match
      case CallDesc.Elem(exc, ret) =>
        val isCtor = ty <= ConstructorT
        if (exc)
          Some(if (isCtor) "function() { throw 0; }" else "() => { throw 0; }")
        else
          exprFor(ret).map { v =>
            if (isCtor) s"function() { return $v; }" else s"() => ($v)"
          }
      case CallDesc.Top => None

  private def isPlainObject(ty: ValueTy): Boolean = ty.record match
    case RecordTy.Elem(map, _) =>
      ObjectT ⊑ ty.copied(record = RecordTy.Elem(map))
    case _ => ObjectT ⊑ ty

  private def propExpr(prop: Property): String = prop match
    case Property.PStr(str) => s"\"${normStr(str)}\""
    case Property.PSym(sym) => s"Symbol.$sym"

  private def propKey(prop: Property): String = s"[${propExpr(prop)}]"

  private val witnesses: List[(ValueTy, List[String])] = List(
    NumberT -> List("0"),
    UndefT -> List("undefined"),
    ObjectT -> List("{}"),
    FunctionT -> List("() => {}"),
    NaNT -> List("NaN"),
    SymbolT -> List("Symbol()"),
    StrT -> List("\"\""),
    TypedArrayT -> List("new Int8Array()"),
    RecordT("OrdinaryObject") -> List("{}"),
    BoolT -> List("true", "false"),
    NumberPosIntT -> List("1"),
    NullT -> List("null"),
    NumberNegIntT -> List("-1"),
    BigIntT -> List("0n"),
    ArrayT -> List("[]"),
    RecordT("ECMAScriptFunctionObject", List("Call", "Construct")) -> List(
      "function(){}",
    ),
    RecordT("ProxyExoticObject", List("Call", "Construct")) -> List(
      "new Proxy(function(){}, {})",
    ),
    RecordT("ProxyExoticObject") -> List("new Proxy({}, {})"),
    RecordT("Set") -> List("new Set()"),
    RecordT("ArrayBuffer") -> List("new ArrayBuffer(0)"),
    RecordT("AsyncGenerator") -> List("(async function*(){})()"),
    RecordT("WeakMap") -> List("new WeakMap()"),
    RecordT("Map") -> List("new Map()"),
    RecordT("WeakSet") -> List("new WeakSet()"),
    RecordT("Generator") -> List("(function*(){})()"),
    RecordT("ArrayIteratorInstance") -> List("[][Symbol.iterator]()"),
    RecordT("BoundFunctionExoticObject", List("Call", "Construct")) -> List(
      "(function(){}).bind()",
    ),
    RecordT("SettledPromise") -> List("Promise.resolve(0)"),
    RecordT("PendingPromise") -> List("new Promise(() => {})"),
    RecordT("Promise") -> List("new Promise(() => {})"),
    RecordT("ErrorObject") -> List("new Error()"),
    RecordT("SharedArrayBuffer") -> List("new SharedArrayBuffer(0)"),
    RecordT("DataView") -> List("new DataView(new ArrayBuffer(0))"),
    RecordT("NumberObject") -> List("Object(0)"),
    RecordT("StringExoticObject") -> List("Object('')"),
    RecordT("BuiltinFunctionObject", List("Call", "Construct")) -> List(
      "Object",
    ),
    RecordT("BuiltinFunctionObject", List("Call")) -> List("Math.max"),
    RegExpT -> List("/./"),
    RecordT("BooleanObject") -> List("Object(true)"),
    RecordT("SymbolObject") -> List("Object(Symbol())"),
    RecordT("BigIntObject") -> List("Object(0n)"),
    RecordT("Date") -> List("new Date()"),
    RecordT("ArgumentsExoticObject") -> List(
      "(function(){ return arguments; })()",
    ),
    RecordT("WeakRef") -> List("new WeakRef({})"),
    RecordT("FinalizationRegistry") -> List(
      "new FinalizationRegistry(() => {})",
    ),
  ) ++ typedArrayEntries
}
