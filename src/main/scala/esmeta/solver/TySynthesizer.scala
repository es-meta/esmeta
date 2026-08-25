package esmeta.solver

import esmeta.cfg.{Branch, Call, CFG, Func}
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.ir.util.UnitWalker
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.ManualInfo
import scala.collection.concurrent.{Map => CMap, TrieMap}
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

class TySynthesizer(cfg: CFG) {

  def candidates(ty: ValueTy): LazyList[String] = Solver.distinct(
    LazyList.from(pinned(ty)) #::: collected(ty) #::: fromStructure(ty) #:::
    fromTemplate(ty) #::: LazyList.from(literalsFor(ty)),
  )

  private def exprFor(ty: ValueTy): Option[String] = candidates(ty).headOption

  // values that are pinned to a type
  private def pinned(ty: ValueTy): List[String] =
    val numbers = ty.number.toNumberSet.fold(Nil) { set =>
      set.toList.sortBy(n => (n.isNaN, n.double)).map(numberLit)
    }
    numbers ++
    ty.bigInt.map(n => s"${n}n").toList ++
    ty.str.map(str => s"\"${normStr(str)}\"").toList ++
    ty.bool.set.toList.sorted.map(b => if (b) "true" else "false") ++
    (if (ty.undef) List("undefined") else Nil) ++
    (if (ty.nullv) List("null") else Nil)

  private def literalsFor(ty: ValueTy): List[String] =
    admitted(specLiterals, ty)

  private def admitted(lits: SpecLiterals, ty: ValueTy): List[String] =
    lits.numbers.filter(ty.number.contains).map(numberLit) ++
    (ty.bigInt match
      case Many => lits.bigInts.map(n => s"${n}n")
      case _    => Nil
    ) ++
    (ty.str match
      case Many => lits.strings.map(s => "\"" + normStr(s) + "\"")
      case _    => Nil
    )

  private def numberLit(n: Number): String =
    val d = n.double
    if (d.isNaN) "NaN"
    else if (d.isPosInfinity) "Infinity"
    else if (d.isNegInfinity) "-Infinity"
    else if (d == 0 && 1 / d < 0) "-0"
    else if (d.isWhole && d.abs <= 9007199254740991.0) d.toLong.toString
    else d.toString

  // values built from the object shape a type carries
  private def fromStructure(ty: ValueTy): LazyList[String] =
    val objs = ty.record match
      case RecordTy.Elem(map, ObjShape(props, call, construct))
          if props.nonEmpty =>
        val ordered = props.toList.sortBy { case (p, _) => propKey(p) }
        val slots = ordered.map { (prop, desc) =>
          val k = propKey(prop)
          if (desc.getExc) LazyList(s"get $k() { throw 0; }")
          else if (desc.setExc) LazyList(s"set $k(_) { throw 0; }")
          else candidates(desc.ty).map(v => s"$k: $v")
        }
        val objs = Solver.oneChange(slots).map(_.mkString("{ ", ", ", " }"))
        if (isPlainObject(ty)) objs
        else
          val traps = ordered match
            case (prop, desc) :: Nil =>
              val key = propExpr(prop)
              val fwd = "return Reflect.get(t, p, r); }"
              if (desc.getExc)
                LazyList(s"get(t, p, r) { if (p === $key) throw 0; $fwd")
              else if (desc.setExc)
                LazyList(
                  s"set(t, p, v, r) { if (p === $key) throw 0; " +
                  "return Reflect.set(t, p, v, r); }",
                )
              else
                candidates(desc.ty)
                  .map(v => s"get(t, p, r) { if (p === $key) return $v; $fwd")
            case _ => LazyList.empty
          val base = exprFor(
            ty.copied(record =
              RecordTy.Elem(map, ObjShape(Map.empty, call, construct)),
            ),
          )
          base match
            case None => LazyList.empty
            case Some(b) =>
              traps.map(h => s"new Proxy($b, { $h })") #:::
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

  // a material stands as written, so its own type must prove the obligation
  private def collected(ty: ValueTy): LazyList[String] =
    lookup(
      ty,
      materialRows.filter { case (wty, _) => wty <= ty },
      cachedMaterials,
    )

  // a template's type settles only once its slots are filled, so prove the
  // obligation against the row with each slot narrowed to what it binds
  private def fromTemplate(ty: ValueTy): LazyList[String] =
    lookup(ty, templateRows.filter(filled(_, ty) <= ty), cachedTemplates)

  private def filled(row: (ValueTy, List[String]), ty: ValueTy): ValueTy =
    val (wty, exprs) = row
    val slots = exprs
      .flatMap(slotPattern.findAllMatchIn(_).map(_.group(1)))
      .distinct
    val record = slots.foldLeft(wty.record) { (r, f) =>
      r.update(f, ty.record(f).value, refine = true)
    }
    wty.copied(record = record)

  private def lookup(
    ty: ValueTy,
    rows: => List[(ValueTy, List[String])],
    cache: CMap[ValueTy, LazyList[String]],
  ): LazyList[String] =
    if (ty.isBottom) LazyList.empty
    else cache.getOrElseUpdate(ty, Solver.prioritize(rows, ty, this))

  private val cachedMaterials = TrieMap[ValueTy, LazyList[String]]()
  private val cachedTemplates = TrieMap[ValueTy, LazyList[String]]()

  // a slot named in a row is filled from what the type binds it to
  private val slotPattern = "\\$([A-Z]\\w*)".r

  def slotChoices(
    template: String,
    ty: ValueTy,
  ): Option[List[(String, LazyList[String])]] =
    val refs =
      slotPattern.findAllMatchIn(template).map(_.group(1)).toList.distinct
    if (refs.exists(!binds(ty, _))) None
    else
      // longest first, so one slot name cannot clobber another's prefix
      val fields = refs.sortBy(-_.length)
      Some(fields.map(field => field -> candidates(ty.record(field).value)))

  // a template fired on an unconstrained slot floods the budget with values
  // the type model merely permits, so only a stated constraint may drive one
  private def binds(ty: ValueTy, field: String): Boolean = ty.record match
    case RecordTy.Elem(map, _) => map.exists((_, fm) => !fm(field).isTop)
    case _                     => false

  // materials taken from the branch conditions of the spec
  private case class SpecLiterals(
    numbers: List[Number],
    bigInts: List[SBigInt],
    strings: List[String],
  )

  private lazy val specLiterals: SpecLiterals = literalsIn(for {
    func <- cfg.funcs
    node <- func.nodes.toList
    branch <- node match
      case branch: Branch => Some(branch.cond)
      case _              => None
  } yield branch)

  private def literalsIn(exprs: List[Expr]): SpecLiterals =
    // how often the spec compares against a literal is evidence of how much it
    // matters, so the counts order what a wide obligation sees first; two maps
    // of numbers because folding yields a BigDecimal, which holds neither NaN
    // nor the infinities and collapses -0 onto 0
    val decimals = MMap[BigDecimal, Int]().withDefaultValue(0)
    val doubles = MMap[Double, Int]().withDefaultValue(0)
    val bigInts = MMap[SBigInt, Int]().withDefaultValue(0)
    val strings = MMap[String, Int]().withDefaultValue(0)
    def bump[T](to: MMap[T, Int], key: T): Unit = to(key) = to(key) + 1
    val walker = new UnitWalker {
      override def walk(expr: Expr): Unit = expr match
        case EStr(str)  => bump(strings, str)
        case EBigInt(n) => bump(bigInts, n)
        case ENumber(d) if !d.isFinite || (d == 0 && 1 / d < 0) =>
          bump(doubles, d)
        case _ =>
          foldLiterals(expr) match
            case Some(n) => bump(decimals, n)
            case None    => super.walk(expr)
      override def walk(ref: Ref): Unit = ref match
        case Field(base, _) => walk(base)
        case _              => super.walk(ref)
    }
    exprs.foreach(walker.walk)
    def ranked[T](from: MMap[T, Int])(using Ordering[T]): List[T] =
      from.toList.sortBy((lit, n) => (-n, lit)).map(_._1)
    val numbers = ranked(decimals).map(n => Number(n.toDouble)) ++
      ranked(doubles).map(Number(_))
    SpecLiterals(numbers, ranked(bigInts), ranked(strings))

  private def foldLiterals(expr: Expr): Option[BigDecimal] = expr match
    case EMath(n)   => Some(n)
    case ENumber(d) => Option.when(!d.isNaN && !d.isInfinite)(BigDecimal(d))
    case EUnary(UOp.Neg, e) => foldLiterals(e).map(-_)
    case EBinary(bop, left, right) =>
      for {
        x <- foldLiterals(left)
        y <- foldLiterals(right)
        z <- bop match
          case BOp.Add => Some(x + y)
          case BOp.Sub => Some(x - y)
          case BOp.Mul => Some(x * y)
          case BOp.Div => Option.when(y != 0)(x / y)
          case BOp.Pow =>
            Option.when(y.isValidInt && y >= 0 && y <= 1024)(x.pow(y.toInt))
          case _ => None
      } yield z
    case _ => None

  // materials observed by running expressions in the reference interpreter

  // run expressions and abstract what each one bound
  private def runAll(exprs: List[String]): Map[String, ValueTy] =
    exprs.grouped(120).flatMap(runBatch).toMap

  private def runBatch(exprs: List[String]): Map[String, ValueTy] =
    if (exprs.isEmpty) Map()
    else
      try {
        val src = ("var __a__ = 0;" :: exprs.zipWithIndex.map { (e, i) =>
          s"var __w${i}__; try { __w${i}__ = ($e); } catch (e) {}"
        }).mkString("\n")
        val st = Interpreter(cfg.init.from(src), timeLimit = Some(20))
        val globals = st.heap.map.collectFirst {
          case (_, m: MapObj) if m.map.contains(Str("__a__")) => m
        }
        (for {
          g <- globals.toList
          (e, i) <- exprs.zipWithIndex
          addr <- g.map.get(Str(s"__w${i}__")).collect { case a: Addr => a }
          obj <- st.heap.map.get(addr).collect { case r: RecordObj => r }
          v <- obj.map.get("Value")
          ty = observedTy(st, v)
          if !ty.isBottom && !ty.record.isBottom
        } yield e -> ty).toMap
      } catch {
        case _: Throwable if exprs.size > 1 =>
          val (l, r) = exprs.splitAt(exprs.size / 2)
          runBatch(l) ++ runBatch(r)
        case _: Throwable => Map()
      }

  // these are on every object, or identify one object rather than its kind
  private val opaque = Set(
    "Prototype",
    "Extensible",
    "PrivateElements",
    "__CODE__",
    "InitialName",
    "Realm",
    "ScriptOrModule",
    "HomeObject",
    "SourceText",
    "Environment",
    "PrivateEnvironment",
  )

  // the field naming the record must land before the ones it declares, so take
  // whichever field narrows the name set most and repeat
  private def refined(ty: RecordTy, fields: List[(String, ValueTy)]): RecordTy =
    fields.iterator
      .map((f, t) => f -> ty.update(f, t, refine = true))
      .filter((_, n) => !n.isBottom)
      .minByOption((_, n) =>
        n.names match
          case Fin(set) => set.size
          case _        => Int.MaxValue,
      ) match
      case None         => ty
      case Some((f, n)) => refined(n, fields.filterNot(_._1 == f))

  private def observedTy(st: State, v: Value): ValueTy = v match
    case addr: Addr =>
      st.heap.map.get(addr) match
        case Some(RecordObj(tname, map)) =>
          val fields = map.iterator
            .filter((f, _) => !opaque(f))
            .map((f, x) => f -> st.typeOf(x, detail = true))
            .toList
          ValueTy(record = refined(RecordTy(tname), fields))
        case Some(o) => st.typeOf(o, detail = false)
        case None    => BotT
    case _ => BotT

  // the spec's creation sites name the constructor; running it on generic
  // values names the argument, which the spec declares only as ESValue
  private lazy val observedCtors: Map[String, String] =
    val names = (for {
      func <- cfg.funcs
      node <- func.nodes.toList
      name <- node match
        case Call(_, ICall(_, EClo(f, _), args), _)
            if f.startsWith("Ordinary") =>
          args.collectFirst { case EStr(p) => p }.flatMap { path =>
            "%(.+)\\.prototype%".r.findFirstMatchIn(path).map(_.group(1))
          }
        case _ => None
    } yield name).distinct
    val args = List(
      "",
      "0",
      "''",
      "true",
      "{}",
      "[]",
      "() => {}",
      "0n",
      "function(){}",
      "{}, {}",
      "function(){}, {}",
    )
    val calls = for { n <- names; a <- args } yield s"new $n($a)"
    runAll(calls).keys
      .groupBy(ctorCall.findFirstMatchIn(_).map(_.group(1)))
      .collect { case (Some(name), calls) => name -> calls.minBy(_.length) }
      .toMap

  // the head of an expression that calls a constructor
  private val ctorCall = "^(?:new )?(\\w+)\\(".r

  // a row calling a constructor takes its argument from what was observed
  private def observed(row: (ValueTy, List[String])): (ValueTy, List[String]) =
    val (ty, exprs) = row
    val next = exprs.map { e =>
      "^(?:new )?(\\w+)\\(".r
        .findFirstMatchIn(e)
        .flatMap { m => observedCtors.get(m.group(1)) }
        .getOrElse(e)
    }
    ty -> next

  // assembly rules: what a type of each kind is built from

  private def namedKinds(base: String, field: String): List[String] =
    ManualInfo.tyModel
      .refinerOf(base)
      .getOrElse(field, Vector())
      .toList
      .flatMap { (ty, sub) =>
        ty.str match
          case One(name) if name == sub => Some(name)
          case _                        => None
      }

  private def declared(tname: String, field: String): ValueTy =
    ManualInfo.tyModel.getField(tname, field).value

  private val typedArrayNames: List[String] =
    namedKinds("TypedArray", "TypedArrayName")

  // 0 to 2 elements: the type domain has no length component, so a length
  // obligation can only be met by enumerating
  private val sizedEntries: List[(ValueTy, List[String])] =
    def each(at: Int => String): List[String] =
      (0 to 2).toList.map(at)
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
      TypedArrayT -> contents(num).map(es =>
        s"new Int8Array([${es.mkString(", ")}])",
      ),
    ) ++ (for (name <- typedArrayNames) yield {
      val order =
        if (declared(name, "ContentType") <= EnumT("bigint")) bigInt else num
      RecordT(name) -> contents(order).map(es =>
        s"new $name([${es.mkString(", ")}])",
      )
    })

  private val typedArrayEntries: List[(ValueTy, List[String])] =
    // InitializeTypedArrayFromArrayBuffer stores its buffer parameter here
    (for (name <- typedArrayNames)
      yield RecordT(name, Map("ViewedArrayBuffer" -> AnyT)) -> List(
        s"new $name($$ViewedArrayBuffer)",
      )) ++
    // the constructor rejects a detached buffer, so detach after building
    (for (name <- typedArrayNames)
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

  private def started(gen: String): String =
    s"(() => { const g = ($gen)(); g.next(); return g; })()"

  private def revokedProxy(target: String): String =
    s"(() => { const r = Proxy.revocable($target, {}); " +
    "r.revoke(); return r.proxy; })()"

  // transferring hands the data to a fresh buffer and detaches the receiver
  private val detachedBuffer: String =
    "(() => { const b = new ArrayBuffer(8); b.transfer(); return b; })()"

  private val authored: List[(ValueTy, List[String])] = List(
    ObjectT -> List("{}"),
    FunctionT -> List("() => {}"),
    SymbolT -> List("Symbol()"),
    RecordT("OrdinaryObject") -> List("{}"),
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
    RecordT("BooleanObject") -> List("Object(true)"),
    RecordT("SymbolObject") -> List("Object(Symbol())"),
    RecordT("BigIntObject") -> List("Object(0n)"),
    RecordT("BuiltinFunctionObject", List("Call", "Construct")) -> List(
      "Object",
    ),
    RecordT("BuiltinFunctionObject", List("Call")) -> List("Math.max"),
    RecordT("ArgumentsExoticObject") -> List(
      "(function(){ return arguments; })()",
    ),
    RecordT("Generator", Map("GeneratorState" -> EnumT("completed"))) -> List(
      started("function*(){}"),
    ),
    RecordT("Generator", Map("GeneratorState" -> EnumT("suspended-yield"))) ->
    List(started("function*(){ yield 0; }")),
    RecordT(
      "AsyncGenerator",
      Map("AsyncGeneratorState" -> EnumT("completed")),
    ) -> List(started("async function*(){}")),
    RecordT(
      "AsyncGenerator",
      Map("AsyncGeneratorState" -> EnumT("draining-queue")),
    ) -> List(started("async function*(){ yield 0; }")),
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
      Map("IteratedArrayLike" -> AnyT, "ArrayLikeIterationKind" -> EnumT("key")),
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
  )

  private lazy val materialRows: List[(ValueTy, List[String])] =
    collectedRows.filterNot(isTemplate)

  private lazy val templateRows: List[(ValueTy, List[String])] =
    collectedRows.filter(isTemplate)

  private def isTemplate(row: (ValueTy, List[String])): Boolean =
    row._2.exists(slotPattern.findFirstIn(_).isDefined)

  private lazy val collectedRows: List[(ValueTy, List[String])] =
    val rows = authored.map(observed) ++ sizedEntries ++ typedArrayEntries
    val runnable =
      rows.flatMap(_._2).filter(slotPattern.findFirstIn(_).isEmpty).distinct
    val obs = runAll(runnable)
    rows.map { (ty, exprs) =>
      val tys = exprs.map(obs.get)
      // observation is evidence the declared type does not carry, but only a
      // row whose every expression ran can claim the join
      val joined =
        if (tys.exists(_.isEmpty)) None
        else tys.flatten.reduceOption(_ || _).filter(_ <= ty)
      joined.getOrElse(ty) -> exprs
    }
}
