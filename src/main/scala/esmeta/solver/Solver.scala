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

  // a row proving the refined field values comes first, then the loose bound
  // decides what applies and the strict one decides what comes next
  private def witnessesFor(ty: ValueTy): List[String] =
    if (ty.isBottom) Nil
    else
      val strict = unrefined(ty)
      val loose = erased(ty)
      cachedWitnesses.getOrElseUpdate(
        (ty, strict, loose), {
          // a field value, unlike its mere presence, needs this tier
          val proven =
            if (ty == strict) Nil
            else witnesses.filter { case (wty, _) => wty <= ty }
          val applies = witnesses.filter { case (wty, _) => wty <= loose }
          val (fit, rest) = applies.partition {
            case (wty, _) => wty <= strict
          }
          val ordered =
            roundRobin(proven.map(_._2)) ++
            roundRobin(fit.map(_._2)) ++
            roundRobin(rest.map(_._2))
          val (templates, plain) =
            ordered.partition(slotRef.findFirstIn(_).isDefined)
          (templates.flatMap(fill(_, ty)) ++ plain).distinct
        },
      )

  // a slot named in a row is filled from what the type binds it to
  private val slotRef = "\\$([A-Z]\\w*)".r

  private val maxSlotCands = 4

  private def fill(template: String, ty: ValueTy): List[String] =
    val refs = slotRef.findAllMatchIn(template).map(_.group(1)).toList.distinct
    if (refs.exists(!binds(ty, _))) Nil
    else
      // longest first, so one slot name cannot clobber another's prefix
      val fields = refs.sortBy(-_.length)
      val slots = fields.map { field =>
        candidates(ty.record(field).value).take(maxSlotCands).toList
      }
      oneChange(slots)
        .map(chosen =>
          fields
            .zip(chosen)
            .foldLeft(template) {
              case (acc, (field, e)) => acc.replace("$" + field, e)
            },
        )
        .toList

  // only a stated constraint, not a type model default, may drive an assembly
  private def binds(ty: ValueTy, field: String): Boolean = ty.record match
    case RecordTy.Elem(map, _) => map.exists((_, fm) => !fm(field).isTop)
    case _                     => false

  // the same bound recurs across every path of every entry
  private val cachedWitnesses =
    collection.concurrent.TrieMap[(ValueTy, ValueTy, ValueTy), List[String]]()

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

  private val typedArrayNames: List[String] = List(
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

  private val sizedEntries: List[(ValueTy, List[String])] =
    def each(at: Int => String): List[String] = (0 to 2).toList.map(at)
    def rep(n: Int, elem: String): String = List.fill(n)(elem).mkString(", ")
    def tuples(order: List[String], n: Int): List[List[String]] =
      if (n == 0) List(Nil)
      else for { e <- order; rest <- tuples(order, n - 1) } yield e :: rest
    def contents(order: List[String]): List[List[String]] =
      (0 to 2).toList.flatMap(tuples(order, _))
    val num = List("0", "1")
    val bigInt = List("0n", "1n")
    List(
      StrT -> each(n => "\"" + "a" * n + "\""),
      ArrayT -> contents(num).map(_.mkString("[", ", ", "]")),
      RecordT("Map") -> each(n => s"new Map([${rep(n, "[0, 0]")}])"),
      RecordT("Set") -> each(n => s"new Set([${rep(n, "0")}])"),
      RecordT("WeakMap") -> each(n => s"new WeakMap([${rep(n, "[{}, 0]")}])"),
      RecordT("WeakSet") -> each(n => s"new WeakSet([${rep(n, "{}")}])"),
      RecordT("ArrayBuffer") -> each(n => s"new ArrayBuffer(${8 * n})"),
      RecordT("ArrayBuffer", Map("ArrayBufferMaxByteLength" -> AnyT)) -> each(
        n => s"new ArrayBuffer(${8 * n}, { maxByteLength: ${8 * n} })",
      ),
      RecordT("SharedArrayBuffer") -> each(n =>
        s"new SharedArrayBuffer(${8 * n})",
      ),
      RecordT("DataView") -> each(n =>
        s"new DataView(new ArrayBuffer(${8 * n}))",
      ),
      TypedArrayT -> contents(num).map(es =>
        s"new Int8Array([${es.mkString(", ")}])",
      ),
    ) ++ (for (name <- typedArrayNames) yield {
      val order =
        if (name.startsWith("BigInt64") || name.startsWith("BigUint64")) bigInt
        else num
      RecordT(name) -> contents(order).map(es =>
        s"new $name([${es.mkString(", ")}])",
      )
    })

  private val typedArrayEntries: List[(ValueTy, List[String])] =
    val names = typedArrayNames
    // InitializeTypedArrayFromArrayBuffer stores its buffer parameter here
    (for (name <- names)
      yield RecordT(name, Map("ViewedArrayBuffer" -> AnyT)) -> List(
        s"new $name($$ViewedArrayBuffer)",
      )) ++
    // the constructor rejects a detached buffer, so detach after building
    (for (name <- names)
      yield RecordT(
        name,
        Map(
          "ViewedArrayBuffer" ->
          RecordT("ArrayBuffer", Map("ArrayBufferData" -> NullT)),
        ),
      ) -> List(
        s"(() => { const b = new ArrayBuffer(8); const t = new $name(b); " +
        "b.transfer(); return t; })()",
      ))

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

  // revoking nulls both slots but keeps the internal methods the target gave
  private def revokedProxy(target: String): String =
    s"(() => { const r = Proxy.revocable($target, {}); " +
    "r.revoke(); return r.proxy; })()"

  // transferring hands the data to a fresh buffer and detaches the receiver
  private val detachedBuffer: String =
    "(() => { const b = new ArrayBuffer(8); b.transfer(); return b; })()"

  private val witnesses: List[(ValueTy, List[String])] = List(
    NumberT -> List("0"),
    UndefT -> List("undefined"),
    ObjectT -> List("{}"),
    FunctionT -> List("() => {}"),
    NaNT -> List("NaN"),
    SymbolT -> List("Symbol()"),
    RecordT("OrdinaryObject") -> List("{}"),
    BoolT -> List("true", "false"),
    NumberPosIntT -> List("1"),
    NullT -> List("null"),
    NumberNegIntT -> List("-1"),
    BigIntT -> List("0n"),
    RecordT("ECMAScriptFunctionObject", List("Call", "Construct")) -> List(
      "function(){}",
    ),
    RecordT("ProxyExoticObject", List("Call", "Construct")) -> List(
      "new Proxy(function(){}, {})",
    ),
    RecordT("ProxyExoticObject") -> List("new Proxy({}, {})"),
    RecordT(
      "ProxyExoticObject",
      Map(
        "Call" -> AnyT,
        "Construct" -> AnyT,
        "ProxyTarget" -> NullT,
        "ProxyHandler" -> NullT,
      ),
    ) -> List(revokedProxy("function(){}")),
    RecordT(
      "ProxyExoticObject",
      Map("ProxyTarget" -> NullT, "ProxyHandler" -> NullT),
    ) -> List(revokedProxy("{}")),
    RecordT("ArrayBuffer", Map("ArrayBufferData" -> NullT)) -> List(
      detachedBuffer,
    ),
    RecordT("AsyncGenerator") -> List("(async function*(){})()"),
    RecordT("Generator") -> List("(function*(){})()"),
    RecordT("ArrayIteratorInstance") -> List("[][Symbol.iterator]()"),
    RecordT("BoundFunctionExoticObject", List("Call", "Construct")) -> List(
      "(function(){}).bind()",
    ),
    RecordT("SettledPromise") -> List("Promise.resolve(0)"),
    RecordT("PendingPromise") -> List("new Promise(() => {})"),
    RecordT("Promise") -> List("new Promise(() => {})"),
    RecordT("ErrorObject") -> List("new Error()"),
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
    RecordT(
      "ProxyExoticObject",
      Map("ProxyTarget" -> AnyT, "ProxyHandler" -> AnyT),
    ) -> List("new Proxy($ProxyTarget, $ProxyHandler)"),
    RecordT("ProxyExoticObject", Map("ProxyTarget" -> AnyT)) -> List(
      "new Proxy($ProxyTarget, {})",
    ),
    RecordT("BoundFunctionExoticObject", Map("BoundTargetFunction" -> AnyT)) ->
    List("($BoundTargetFunction).bind()"),
    RecordT(
      "ArrayIteratorInstance",
      Map(
        "IteratedArrayLike" -> AnyT,
        "ArrayLikeIterationKind" -> EnumT("key+value"),
      ),
    ) -> List("Array.prototype.entries.call($IteratedArrayLike)"),
    RecordT(
      "ArrayIteratorInstance",
      Map(
        "IteratedArrayLike" -> AnyT,
        "ArrayLikeIterationKind" -> EnumT("key"),
      ),
    ) -> List("Array.prototype.keys.call($IteratedArrayLike)"),
    RecordT(
      "ArrayIteratorInstance",
      Map(
        "IteratedArrayLike" -> AnyT,
        "ArrayLikeIterationKind" -> EnumT("value"),
      ),
    ) -> List("Array.prototype.values.call($IteratedArrayLike)"),
    // an unconstrained kind admits any of the three, so enumerate them
    RecordT("ArrayIteratorInstance", Map("IteratedArrayLike" -> AnyT)) -> List(
      "Array.prototype.values.call($IteratedArrayLike)",
      "Array.prototype.keys.call($IteratedArrayLike)",
      "Array.prototype.entries.call($IteratedArrayLike)",
    ),
    RecordT("FinalizationRegistry") -> List(
      "new FinalizationRegistry(() => {})",
    ),
  ) ++ sizedEntries ++ typedArrayEntries
}
