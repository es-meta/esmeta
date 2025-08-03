package esmeta.es.builtin

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.spec.*
import esmeta.ty.*
import esmeta.util.BaseUtils.*

/** model for intrinsics */
case class Intrinsics(cfg: CFG) {

  /** shortcuts */
  private def spec = cfg.program.spec
  given CFG = cfg

  /** get map for heap */
  lazy val map: Map[Addr, Obj] = {
    var _map = Map[Addr, Obj]()

    // add intrinsic objects
    intrinsics.foreach {
      case (name, Struct(typeName, imap, nmap))
          if !(yets contains name.split("\\.").head) =>
        // base object
        _map += intrAddr(name) -> recordObj(typeName)(
          List(
            INNER_MAP -> mapAddr(intrName(name)),
            PRIVATE_ELEMENTS -> elemsAddr(intrName(name)),
          ) ++ imap: _*,
        )

        // map object
        _map ++= getMapObjects(intrName(name), name, nmap)
      case _ =>
    }

    // not yet objects
    yets.foreach { (name, _) => _map += (intrAddr(name) -> YetObj(name, name)) }

    // result
    _map
  }

  lazy val kinds: Map[String, ValueTy] =
    val xs = (for {
      x <- names.toList
      addr = intrAddr(x)
      case (obj: RecordObj) <- map.get(addr)
      ty =
        if (obj.map contains "Construct") ConstructorT
        else if (obj.map contains "Call") FunctionT
        else ObjectT
    } yield x -> ty).toMap ++ yets
    xs.map { case (x, ty) => s"%$x%" -> ty }

  val names: Set[String] = intrinsics.keySet ++ yets.keySet

  /** get intrinsic map */
  val obj: MapObj = MapObj(
    names.toList.map(x => Str(s"%$x%") -> intrAddr(x)),
  )

  // get closures
  private def clo(name: String): Clo = Clo(cfg.fnameMap(name), Map())
  private def intrClo(name: String): Clo = clo(intrName(name))

  // intrinsics
  // https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects
  private lazy val intrinsics: Map[String, Struct] = errors ++ Map(
    "print" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "ScriptOrModule" -> Null,
        "Realm" -> realmAddr,
        INNER_CODE -> intrClo("print"),
        "Prototype" -> intrAddr("Function.prototype"),
        "InitialName" -> Str("print"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(0.0), F, F, T),
      ),
    ),
    "Object" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("Object.prototype"), F, F, F),
      ),
    ),
    "Object.prototype" -> Struct(
      typeName = "ImmutablePrototypeExoticObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> Null,
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Object"), T, F, T),
      ),
    ),
    "Object.assign" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "Function" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("Function.prototype"), F, F, F),
      ),
    ),
    "Function.prototype" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
        INNER_CODE -> intrClo("Function.prototype"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(0.0), F, F, T),
        PropKey.Str("name") -> DataDesc(Str(""), F, F, T),
        PropKey.Str("constructor") -> DataDesc(intrAddr("Function"), T, F, T),
        PropKey.Sym("hasInstance") -> DataDesc(
          intrAddr("Function.prototype[%Symbol.hasInstance%]"),
          F,
          F,
          F,
        ),
      ),
    ),
    "Boolean" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("Boolean.prototype"), F, F, F),
      ),
    ),
    "Boolean.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "BooleanData" -> Bool(false),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Boolean"), T, F, T),
      ),
    ),
    "Symbol" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("asyncIterator") -> DataDesc(
          symbolAddr("asyncIterator"),
          F,
          F,
          F,
        ),
        PropKey
          .Str("hasInstance") -> DataDesc(symbolAddr("hasInstance"), F, F, F),
        PropKey.Str("isConcatSpreadable") -> DataDesc(
          symbolAddr("isConcatSpreadable"),
          F,
          F,
          F,
        ),
        PropKey.Str("iterator") -> DataDesc(symbolAddr("iterator"), F, F, F),
        PropKey.Str("match") -> DataDesc(symbolAddr("match"), F, F, F),
        PropKey.Str("matchAll") -> DataDesc(symbolAddr("matchAll"), F, F, F),
        PropKey.Str("replace") -> DataDesc(symbolAddr("replace"), F, F, F),
        PropKey.Str("search") -> DataDesc(symbolAddr("search"), F, F, F),
        PropKey.Str("species") -> DataDesc(symbolAddr("species"), F, F, F),
        PropKey.Str("split") -> DataDesc(symbolAddr("split"), F, F, F),
        PropKey
          .Str("toPrimitive") -> DataDesc(symbolAddr("toPrimitive"), F, F, F),
        PropKey
          .Str("toStringTag") -> DataDesc(symbolAddr("toStringTag"), F, F, F),
        PropKey
          .Str("unscopables") -> DataDesc(symbolAddr("unscopables"), F, F, F),
        PropKey.Str("prototype") -> DataDesc(symbolAddr("prototype"), F, F, F),
      ),
    ),
    "Symbol.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Symbol"), T, F, T),
        PropKey.Sym("toPrimitive") ->
        DataDesc(
          intrAddr("Symbol.prototype[%Symbol.toPrimitive%]"),
          F,
          F,
          T,
        ),
        PropKey.Sym("toStringTag") -> DataDesc(Str("Symbol"), F, F, T),
      ),
    ),
    "Error" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("Error.prototype"), F, F, F),
      ),
    ),
    "Error.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Error"), T, F, T),
        PropKey.Str("message") -> DataDesc(Str(""), T, F, T),
        PropKey.Str("name") -> DataDesc(Str("Error"), T, F, T),
      ),
    ),
    "Number" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("EPSILON") -> DataDesc(Number(math.ulp(1.0)), F, F, F),
        PropKey.Str("MAX_SAFE_INTEGER") -> DataDesc(
          Number(9007199254740991.0),
          F,
          F,
          F,
        ),
        PropKey.Str("MAX_VALUE") -> DataDesc(Number(Double.MaxValue), F, F, F),
        PropKey.Str("MIN_SAFE_INTEGER") ->
        DataDesc(Number(-9007199254740991.0), F, F, F),
        PropKey.Str("MIN_VALUE") -> DataDesc(
          Number(Double.MinPositiveValue),
          F,
          F,
          F,
        ),
        PropKey.Str("NaN") -> DataDesc(Number(Double.NaN), F, F, F),
        PropKey.Str("NEGATIVE_INFINITY") ->
        DataDesc(Number(Double.NegativeInfinity), F, F, F),
        PropKey.Str("parseFloat") -> DataDesc(intrAddr("parseFloat"), T, F, T),
        PropKey.Str("parseInt") -> DataDesc(intrAddr("parseInt"), T, F, T),
        PropKey.Str("POSITIVE_INFINITY") ->
        DataDesc(Number(Double.PositiveInfinity), F, F, F),
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("Number.prototype"), F, F, F),
      ),
    ),
    "Number.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "NumberData" -> Number(0.0),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Number"), T, F, T),
      ),
    ),
    "Number.prototype.toString" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
      ),
    ),
    "BigInt" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("BigInt.prototype"), F, F, F),
      ),
    ),
    "BigInt.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("BigInt"), T, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("BigInt"), F, F, T),
      ),
    ),
    "Math" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("E") -> DataDesc(Number(2.7182818284590452354), F, F, F),
        PropKey.Str("LN10") -> DataDesc(Number(2.302585092994046), F, F, F),
        PropKey.Str("LN2") -> DataDesc(Number(0.6931471805599453), F, F, F),
        PropKey.Str("LOG10E") -> DataDesc(Number(0.4342944819032518), F, F, F),
        PropKey.Str("LOG2E") -> DataDesc(Number(1.4426950408889634), F, F, F),
        PropKey.Str("PI") -> DataDesc(Number(3.1415926535897932), F, F, F),
        PropKey.Str("SQRT1_2") -> DataDesc(Number(0.7071067811865476), F, F, F),
        PropKey.Str("SQRT2") -> DataDesc(Number(1.4142135623730951), F, F, F),
        PropKey.Sym("toStringTag") -> DataDesc(Str("Math"), F, F, T),
      ),
    ),
    "Math.hypot" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "Math.max" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "Math.min" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "Date" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(7.0), F, F, T),
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("Date.prototype"), F, F, F),
      ),
    ),
    "Date.UTC" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(7.0), F, F, T),
      ),
    ),
    "Date.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Date"), T, F, T),
      ),
    ),
    "Date.prototype.setFullYear" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(3.0), F, F, T),
      ),
    ),
    "Date.prototype.setHours" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(4.0), F, F, T),
      ),
    ),
    "Date.prototype.setMinutes" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(3.0), F, F, T),
      ),
    ),
    "Date.prototype.setMonth" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "Date.prototype.setSeconds" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCFullYear" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(3.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCHours" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(4.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCMinutes" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(3.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCMonth" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCSeconds" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "String" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("String.prototype"), F, F, F),
      ),
    ),
    "String.fromCharCode" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
      ),
    ),
    "String.fromCodePoint" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
      ),
    ),
    "String.prototype" -> Struct(
      typeName = "StringExoticObject",
      imap = List(
        "Extensible" -> Bool(true),
        "StringData" -> Str(""),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(0.0), F, F, F),
        PropKey.Str("constructor") -> DataDesc(intrAddr("String"), T, F, T),
      ),
    ),
    "String.prototype.concat" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
      ),
    ),
    "StringIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Iterator.prototype"),
      ),
      nmap = List(
        PropKey.Sym("toStringTag") -> DataDesc(Str("String Iterator"), F, F, T),
      ),
    ),
    "RegExp" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("RegExp.prototype"), F, F, F),
      ),
    ),
    "RegExp.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("RegExp"), T, F, T),
      ),
    ),
    "Array" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("Array.prototype"), F, F, F),
      ),
    ),
    "Array.prototype" -> Struct(
      typeName = "Array",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(0.0), T, F, F),
        PropKey.Str("constructor") -> DataDesc(intrAddr("Array"), T, F, T),
        PropKey.Sym("iterator") ->
        DataDesc(intrAddr("Array.prototype.values"), T, F, T),
        PropKey.Sym("unscopables") ->
        DataDesc(intrAddr("Array.prototype[%Symbol.unscopables%]"), F, F, T),
      ),
    ),
    "Array.prototype.concat" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype.push" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype.unshift" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype[%Symbol.unscopables%]" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> Null,
      ),
      nmap = List(
        PropKey.Str("at") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("copyWithin") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("entries") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("fill") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("find") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("findIndex") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("findLast") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("findLastIndex") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("flat") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("flatMap") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("includes") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("keys") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("toReversed") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("toSorted") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("toSpliced") -> DataDesc(Bool(true), T, T, T),
        PropKey.Str("values") -> DataDesc(Bool(true), T, T, T),
      ),
    ),
    "ArrayIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Iterator.prototype"),
      ),
      nmap = List(
        PropKey.Sym("toStringTag") -> DataDesc(Str("Array Iterator"), F, F, T),
      ),
    ),
    "Map" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("prototype") -> DataDesc(intrAddr("Map.prototype"), F, F, F),
      ),
    ),
    "Map.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Map"), T, F, T),
        PropKey.Sym("iterator") ->
        DataDesc(intrAddr("Map.prototype.entries"), T, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("Map"), F, F, T),
      ),
    ),
    "MapIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Iterator.prototype"),
      ),
      nmap = List(
        PropKey.Sym("toStringTag") -> DataDesc(Str("Map Iterator"), F, F, T),
      ),
    ),
    "Set" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("prototype") -> DataDesc(intrAddr("Set.prototype"), F, F, F),
      ),
    ),
    "Set.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Set"), T, F, T),
        PropKey
          .Str("keys") -> DataDesc(intrAddr("Set.prototype.values"), T, F, T),
        PropKey.Sym("iterator") -> DataDesc(
          intrAddr("Set.prototype.values"),
          T,
          F,
          T,
        ),
        PropKey.Sym("toStringTag") -> DataDesc(Str("Set"), F, F, T),
      ),
    ),
    "SetIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Iterator.prototype"),
      ),
      nmap = List(
        PropKey.Sym("toStringTag") -> DataDesc(Str("Set Iterator"), F, F, T),
      ),
    ),
    "WeakMap" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("WeakMap.prototype"), F, F, F),
      ),
    ),
    "WeakMap.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("WeakMap"), T, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("WeakMap"), F, F, T),
      ),
    ),
    "WeakSet" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("WeakSet.prototype"), F, F, F),
      ),
    ),
    "WeakSet.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("WeakSet"), T, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("WeakSet"), F, F, T),
      ),
    ),
    "ArrayBuffer" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("prototype") -> DataDesc(
          intrAddr("ArrayBuffer.prototype"),
          F,
          F,
          F,
        ),
      ),
    ),
    "ArrayBuffer.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey
          .Str("constructor") -> DataDesc(intrAddr("ArrayBuffer"), T, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("ArrayBuffer"), F, F, T),
      ),
    ),
    "JSON" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Sym("toStringTag") -> DataDesc(Str("JSON"), F, F, T),
      ),
    ),
    "JSON.parse" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(2.0), F, F, T),
      ),
    ),
    "JSON.stringify" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(3.0), F, F, T),
      ),
    ),
    "Map.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Map"), T, F, T),
        PropKey.Sym("iterator") ->
        DataDesc(intrAddr("Map.prototype.entries"), T, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("Map"), F, F, T),
      ),
    ),
    "Iterator.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> AccessorDesc(
          intrAddr("get Iterator.prototype.constructor"),
          intrAddr("set Iterator.prototype.constructor"),
          F,
          T,
        ),
        PropKey.Sym("toStringTag") -> AccessorDesc(
          intrAddr("get Iterator.prototype[%Symbol.toStringTag%]"),
          intrAddr("set Iterator.prototype[%Symbol.toStringTag%]"),
          F,
          T,
        ),
      ),
    ),
    "ForInIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Prototype" -> intrAddr("Iterator.prototype"),
      ),
    ),
    "AsyncIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
    ),
    "AsyncFromSyncIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("AsyncIteratorPrototype"),
      ),
    ),
    "Promise" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey
          .Str("prototype") -> DataDesc(intrAddr("Promise.prototype"), F, F, F),
      ),
    ),
    "Promise.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(intrAddr("Promise"), T, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("Promise"), F, F, T),
      ),
    ),
    "GeneratorFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
        PropKey.Str("prototype") ->
        DataDesc(intrAddr("GeneratorFunction.prototype"), F, F, F),
      ),
    ),
    "GeneratorFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") -> DataDesc(
          intrAddr("GeneratorFunction"),
          F,
          F,
          T,
        ),
        PropKey.Str("prototype") -> DataDesc(
          intrAddr("GeneratorPrototype"),
          F,
          F,
          T,
        ),
        PropKey.Sym("toStringTag") -> DataDesc(
          Str("GeneratorFunction"),
          F,
          F,
          T,
        ),
      ),
    ),
    "AsyncGeneratorFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
        PropKey.Str("prototype") ->
        DataDesc(intrAddr("AsyncGeneratorFunction.prototype"), F, F, F),
      ),
    ),
    "AsyncGeneratorFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") ->
        DataDesc(intrAddr("AsyncGeneratorFunction"), F, F, T),
        PropKey.Str("prototype") ->
        DataDesc(
          intrAddr("AsyncGeneratorPrototype"),
          F,
          F,
          T,
        ),
        PropKey.Sym("toStringTag") -> DataDesc(
          Str("AsyncGeneratorFunction"),
          F,
          F,
          T,
        ),
      ),
    ),
    // Generator.prototype == GeneratorFunction.prototype.prototype == GeneratorPrototype
    "GeneratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Iterator.prototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") ->
        DataDesc(intrAddr("GeneratorFunction.prototype"), F, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("Generator"), F, F, T),
      ),
    ),
    // AsyncGenerator.prototype == AsyncGeneratorFunction.prototype.prototype == AsyncGeneratorPrototype
    "AsyncGeneratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("AsyncIteratorPrototype"),
      ),
      nmap = List(
        PropKey.Str("constructor") ->
        DataDesc(intrAddr("AsyncGeneratorFunction.prototype"), F, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("AsyncGenerator"), F, F, T),
      ),
    ),
    "AsyncFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
        PropKey.Str("name") -> DataDesc(Str("AsyncFunction"), F, F, T),
        PropKey.Str("prototype") ->
        DataDesc(intrAddr("AsyncFunction.prototype"), F, F, F),
      ),
    ),
    "AsyncFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        PropKey
          .Str("constructor") -> DataDesc(intrAddr("AsyncFunction"), F, F, T),
        PropKey.Sym("toStringTag") -> DataDesc(Str("AsyncFunction"), F, F, T),
      ),
    ),
    "Reflect" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        PropKey.Sym("toStringTag") -> DataDesc(Str("Reflect"), F, F, T),
      ),
    ),
    "Proxy" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
    ),
    "AggregateError" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Error"),
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("prototype") -> DataDesc(
          intrAddr("AggregateError.prototype"),
          F,
          F,
          F,
        ),
      ),
    ),
    "AggregateError.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Error.prototype"),
      ),
      nmap = List(
        PropKey
          .Str("constructor") -> DataDesc(intrAddr("AggregateError"), T, F, T),
        PropKey.Str("message") -> DataDesc(Str(""), T, F, T),
        PropKey.Str("name") -> DataDesc(Str("AggregateError"), T, F, T),
      ),
    ),
    "ThrowTypeError" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(false),
      ),
      nmap = List(
        PropKey.Str("length") -> DataDesc(Number(0.0), F, F, F),
        PropKey.Str("name") -> DataDesc(Str(""), F, F, F),
      ),
    ),
    "Iterator" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        PropKey.Str("prototype") -> DataDesc(
          intrAddr("Iterator.prototype"),
          F,
          F,
          F,
        ),
        PropKey.Sym("toStringTag") -> DataDesc(Str("Iterator"), F, F, T),
      ),
    ),
    "IteratorHelperPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Iterator.prototype"),
      ),
      nmap = List(
        PropKey.Sym("toStringTag") -> DataDesc(Str("Iterator Helper"), F, F, T),
      ),
    ),
    "WrapForValidIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Prototype" -> intrAddr("Iterator.prototype"),
      ),
    ),
  )

  // error constructors
  private def errList: List[String] = List(
    "EvalError",
    "RangeError",
    "ReferenceError",
    "SyntaxError",
    "TypeError",
    "URIError",
  )

  // error intrinsics
  private def errors: Map[String, Struct] = (for {
    name <- errList
    (name, struct) <- getErrorMap(name)
  } yield name -> struct).toMap

  private def getErrorMap(errName: String): Map[String, Struct] =
    Map(
      s"$errName" -> Struct(
        typeName = "BuiltinFunctionObject",
        imap = List(
          "Extensible" -> Bool(true),
          "ScriptOrModule" -> Null,
          "Realm" -> realmAddr,
          INNER_CODE -> intrClo(errName),
          "Prototype" -> intrAddr("Error"),
          "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
          "InitialName" -> Str(errName),
        ),
        nmap = List(
          PropKey.Str("length") -> DataDesc(Number(1.0), F, F, T),
          PropKey.Str("name") -> DataDesc(Str(errName), F, F, T),
          PropKey.Str("prototype") -> DataDesc(
            intrAddr(s"$errName.prototype"),
            F,
            F,
            F,
          ),
        ),
      ),
      s"$errName.prototype" -> Struct(
        typeName = "OrdinaryObject",
        imap = List(
          "Extensible" -> Bool(true),
          "Prototype" -> intrAddr("Error.prototype"),
        ),
        nmap = List(
          PropKey.Str("constructor") -> DataDesc(intrAddr(errName), T, F, T),
          PropKey.Str("message") -> DataDesc(Str(""), T, F, T),
          PropKey.Str("name") -> DataDesc(Str(errName), T, F, T),
        ),
      ),
    )
}
