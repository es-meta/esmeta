package esmeta.solver

import esmeta.cfg.Func
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
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
    val rest = st.getConstr(SArgs.sym) // TODO: handle variadic parameters
    val newTarget = st.getConstr(SNewTarget.sym)
    val len = entryFunc.head match {
      case Some(h: BuiltinHead) => h.arity._2
      case _                    => 0
    }
    val args = (0 until len).toList.map(i => st.getConstr(i))
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

  // get a JavaScript expression representing the value type
  def getJSExpr(ty: ValueTy): Option[String] = exprFor(ty)

  // get a JavaScript expression representing the newTarget value type
  def getNewTargetExpr(ty: ValueTy): Option[String] =
    if (UndefT ⊑ ty) None else getJSExpr(ty)

  // ---------------------------------------------------------------------------
  // candidate enumeration
  // ---------------------------------------------------------------------------
  def candidates(ty: ValueTy): LazyList[String] =
    val base = exprFor(ty).iterator.to(LazyList)
    val objects =
      if (isBasePlainObject(ty)) objectCandidates(ty) else LazyList.empty
    val extra =
      extraCands.collect { case (isTy, js) if isTy(ty) => js }.to(LazyList)
    distinct(base #::: objects #::: extra)

  def newTargetCandidates(ty: ValueTy): List[String] =
    if (UndefT ⊑ ty) List("")
    else candidates(ty).toList

  def oneChange(slots: List[List[String]]): LazyList[List[String]] =
    if (slots.exists(_.isEmpty)) LazyList.empty
    else
      val heads = slots.map(_.head)
      val variants =
        for {
          (slot, i) <- slots.iterator.zipWithIndex
          alt <- slot.tail
        } yield heads.updated(i, alt)
      heads #:: variants.to(LazyList)

  private def distinct(xs: LazyList[String]): LazyList[String] =
    val seen = scala.collection.mutable.Set[String]()
    xs.filter(seen.add)

  private val extraCands: List[(ValueTy => Boolean, String)] =
    def isNum(v: Double): ValueTy => Boolean = _.number.contains(Number(v))
    def isAnyNum: ValueTy => Boolean = !_.number.isBottom
    def isBig(v: BigInt): ValueTy => Boolean = _.bigInt.contains(v)
    def isAnyBig: ValueTy => Boolean = !_.bigInt.isBottom
    def isBool(b: Boolean): ValueTy => Boolean = _.bool.contains(b)
    def isStr(s: String): ValueTy => Boolean = _.str.contains(s)
    def isAnyStr: ValueTy => Boolean = !_.str.isBottom
    def isUndef: ValueTy => Boolean = !_.undef.isBottom
    def isNull: ValueTy => Boolean = !_.nullv.isBottom
    def isSym: ValueTy => Boolean = ty => SymbolT ⊑ ty || (ty overlap SymbolT)
    def isObj: ValueTy => Boolean = ty => ObjectT ⊑ ty
    List(
      (isUndef, "undefined"),
      (isNull, "null"),
      (isBool(true), "true"),
      (isBool(false), "false"),
      (isNum(0), "0"),
      (isNum(1), "1"),
      (isNum(-0.0), "-0"),
      (isNum(-1), "-1"),
      (isNum(0.1), "0.1"),
      (isNum(-0.1), "-0.1"),
      ((_: ValueTy).number.contains(Number.NaN), "NaN"),
      ((_: ValueTy).number.contains(Number.Inf), "Infinity"),
      ((_: ValueTy).number.contains(-Number.Inf), "-Infinity"),
      (isAnyNum, "Number.MAX_SAFE_INTEGER"),
      (isAnyNum, "Number.MIN_SAFE_INTEGER"),
      (isAnyNum, "Number.MAX_VALUE"),
      (isAnyNum, "-Number.MAX_VALUE"),
      (isBig(BigInt(0)), "0n"),
      (isBig(BigInt(1)), "1n"),
      (isBig(BigInt(-1)), "-1n"),
      (isAnyBig, "9223372036854775807n"),
      (isAnyBig, "-9223372036854775808n"),
      (isAnyBig, "18446744073709551615n"),
      (isSym, "Symbol()"),
      (isSym, "Symbol.iterator"),
      (isStr(""), "\"\""),
      (isStr("0"), "\"0\""),
      (isAnyStr, "\"\""),
      (isObj, "[]"),
      (isObj, "{}"),
      (isObj, "function(){}"),
      (isObj, "Object.freeze({ x: 1 })"),
      (isObj, "Object.seal({ x: 1 })"),
      (isObj, "Object.preventExtensions({})"),
      (isObj, "Object.create(null)"),
    )

  private def objectCandidates(ty: ValueTy): LazyList[String] =
    ty.record match
      case RecordTy.Elem(_, ObjShape(props, _, _)) if props.nonEmpty =>
        val ordered = props.toList.sortBy { case (p, _) => propKey(p) }
        val slots: List[List[String]] = ordered.map { (prop, desc) =>
          val k = propKey(prop)
          if (desc.getExc) List(s"get $k() { throw 0; }")
          else if (desc.setExc) List(s"set $k(_) { throw 0; }")
          else candidates(desc.ty).toList.map(v => s"$k: $v")
        }
        oneChange(slots).map(_.mkString("{ ", ", ", " }"))
      case _ => LazyList.empty

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

  def exprFor(ty: ValueTy): Option[String] =
    if (ty.number.contains(Number(0))) Some("0")
    else if (ty.number.contains(Number(-1))) Some("-1")
    else if (ty.number.contains(Number(1))) Some("1")
    else if (ty.number.contains(Number.NaN)) Some("NaN")
    else if (ty.number.contains(Number.Inf)) Some("Infinity")
    else if (ty.number.contains(-Number.Inf)) Some("-Infinity")
    else if (!ty.undef.isBottom) Some("undefined")
    else if (!ty.nullv.isBottom) Some("null")
    else if (ty.str.contains("")) Some("\"\"")
    else if (ty.bool.contains(false)) Some("false")
    else if (ty.bool.contains(true)) Some("true")
    else if (ty.bigInt.contains(BigInt(0))) Some("0n")
    else if (ty.bigInt.contains(BigInt(1))) Some("1n")
    else defaultFor(ty)

  def defaultFor(ty: ValueTy): Option[String] =
    if (ty.isBottom) None
    else
      objectWithProps(ty)
        .orElse(constructDescExpr(ty))
        .orElse(callDescExpr(ty))
        .orElse(baseDefaultFor(ty))

  private def constructDescExpr(ty: ValueTy): Option[String] =
    ty.record.construct match
      case ConstructDesc.Elem(exc, ret) =>
        if (exc) Some("function() { throw 0; }")
        else exprFor(ret).map(v => s"function() { return $v; }")
      case ConstructDesc.Top => None

  private def callDescExpr(ty: ValueTy): Option[String] =
    ty.record.call match
      case CallDesc.Elem(exc, ret) =>
        val isConstructor = ty <= ConstructorT
        if (exc)
          Some(
            if (isConstructor) "function() { throw 0; }"
            else "() => { throw 0; }",
          )
        else
          exprFor(ret).map { v =>
            if (isConstructor) s"function() { return $v; }"
            else s"() => ($v)"
          }
      case CallDesc.Top => None

  private def baseDefaultFor(ty: ValueTy): Option[String] =
    if (ty.isBottom) None
    else if (ty == ObjectT) Some("{}")
    else
      defaults
        .collectFirst { case (tyCase, js) if tyCase ⊑ ty => js }
        .orElse(defaults.collectFirst {
          case (tyCase, js) if ty overlap tyCase => js
        })

  private def isBasePlainObject(ty: ValueTy): Boolean = ty.record match
    case RecordTy.Elem(map, _) =>
      ObjectT ⊑ ty.copied(record = RecordTy.Elem(map))
    case _ => ObjectT ⊑ ty

  private def objectWithProps(ty: ValueTy): Option[String] =
    if (!isBasePlainObject(ty)) None
    else
      val props = properties(ty)
      if (props.nonEmpty && props.forall(_.isDefined))
        Some(props.flatten.mkString("{ ", ", ", " }"))
      else None

  private def properties(ty: ValueTy): List[Option[String]] =
    ty.record match
      case RecordTy.Elem(_, ObjShape(props, _, _)) =>
        // TODO call/construct descriptors
        props.toList.map { (prop, desc) =>
          val k = propKey(prop)
          if (desc.getExc) Some(s"get $k() { throw 0; }")
          else if (desc.setExc) Some(s"set $k(_) { throw 0; }")
          else if (!desc.ty.isBottom) exprFor(desc.ty).map(v => s"$k: $v")
          else None
        }
      case _ => Nil

  private def propKey(prop: Property): String = prop match
    case Property.PStr(str) => str
    case Property.PSym(sym) => s"[Symbol.$sym]"

  private val defaults: List[(ValueTy, String)] = List(
    SymbolT -> "Symbol()",
    ConstructorT -> "function() {}",
    FunctionT -> "() => {}",
    RecordT("ProxyExoticObject") -> "new Proxy({}, {})",
    RecordT("BoundFunctionExoticObject") -> "(function(){}).bind()",
    RecordT("BuiltinFunctionObject") -> "Math.max",
    ArrayT -> "[]",
    TypedArrayT -> "new Int8Array()",
    RecordT("BigInt64Array") -> "new BigInt64Array()",
    RecordT("BigUint64Array") -> "new BigUint64Array()",
    RecordT("ArrayIteratorInstance") -> "[][Symbol.iterator]()",
    RegExpT -> "/./",
    RecordT("BooleanObject") -> "Object(true)",
    RecordT("NumberObject") -> "Object(0)",
    RecordT("StringExoticObject") -> "Object('')",
    RecordT("SymbolObject") -> "Object(Symbol())",
    RecordT("BigIntObject") -> "Object(0n)",
    RecordT("Map") -> "new Map()",
    RecordT("Set") -> "new Set()",
    RecordT("WeakMap") -> "new WeakMap()",
    RecordT("WeakSet") -> "new WeakSet()",
    RecordT("ArrayBuffer") -> "new ArrayBuffer(0)",
    RecordT("SharedArrayBuffer") -> "new SharedArrayBuffer(0)",
    RecordT("DataView") -> "new DataView(new ArrayBuffer(0))",
    RecordT("Date") -> "new Date()",
    RecordT("Promise") -> "new Promise(() => {})",
    RecordT("ErrorObject") -> "new Error()",
    RecordT("Generator") -> "(function*(){})()",
    RecordT("AsyncGenerator") -> "(async function*(){})()",
    RecordT("WeakRef") -> "new WeakRef({})",
    RecordT("FinalizationRegistry") -> "new FinalizationRegistry(() => {})",
    RecordT("ArgumentsExoticObject") -> "(function(){ return arguments; })()",
    ObjectT -> "{}",
  )
}
