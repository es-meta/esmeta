package esmeta.js.builtin

import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*

/** model for intrinsics */
case class Intrinsics(cfg: CFG) {

  /** shortcuts */
  private val T = true
  private val F = false
  private val U = Undef
  private def spec = cfg.program.spec
  given CFG = cfg

  /** get map for heap */
  lazy val map: Map[Addr, Obj] = {
    var _map = Map[Addr, Obj]()
    intrinsics.foreach(_map ++= _.toMap)
    _map
  }

  /** get intrinsic record */
  def obj: MapObj = MapObj("Record")((for {
    row <- spec.tables(WELL_KNOWN_INTRINSICS).rows
    intrKey <- row.headOption.map(_.trim.replace("%", ""))
  } yield Str(intrKey) -> intrAddr(intrKey)): _*)

  /** extensions for builtin model structure */
  // TODO refactoring
  extension (pair: (String, Struct)) {

    /** convert builtin model structure to heap objects */
    def toMap: Map[Addr, Obj] = {
      val (name, Struct(typeName, imap, nmap)) = pair
      var map = Map[Addr, Obj]()

      // base object
      map += intrAddr(name) -> MapObj(typeName)(
        (SUBMAP -> submapAddr(intrName(name)) :: imap).map {
          case (k, v) => Str(k) -> v
        }: _*,
      )

      // submap object
      map ++= getSubmapObjects(intrName(name), nmap)

      map
    }
  }

  // get closures
  private def clo(name: String): Clo = Clo(cfg.fnameMap(name), Map())
  private def intrClo(name: String): Clo = clo(intrName(name))

  // https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects
  //
  // Unless specified otherwise, the [[Extensible]] internal slot of a built-in object initially has the value true
  // [[Extensible]] = true
  //
  // Every built-in function object has a [[Realm]] internal slot whose value is the Realm Record of the realm for which the object was initially created.
  // [[Realm]] = realm
  //
  // Unless otherwise specified every built-in function and every built-in constructor has the Function prototype object, which is the initial value of the expression Function.prototype (20.2.3), as the value of its [[Prototype]] internal slot.
  // For constructor, [[Prototype]] = Function.prototype
  //
  // Unless otherwise specified every built-in prototype object has the Object prototype object, which is the initial value of the expression Object.prototype (20.1.3), as the value of its [[Prototype]] internal slot, except the Object prototype object itself.
  // For prototype object, [[Prototype]] = Object.prototype
  //
  // Every built-in function object, including constructors, has a "length" property whose value is a non-negative integral Number. Unless otherwise specified, this value is equal to the number of required parameters shown in the subclause heading for the function description. Optional parameters and rest parameters are not included in the parameter count.
  // Unless otherwise specified, the "length" property of a built-in function object has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
  // .length = DataProperty { ... }
  //
  // Every built-in function object, including constructors, has a "name" property whose value is a String. Unless otherwise specified, this value is the name that is given to the function in this specification. Functions that are identified as anonymous functions use the empty String as the value of the "name" property. For functions that are specified as properties of objects, the name value is the property name string used to access the function. Functions that are specified as get or set accessor functions of built-in properties have "get" or "set" (respectively) passed to the prefix parameter when calling CreateBuiltinFunction.
  // The value of the "name" property is explicitly specified for each built-in functions whose property key is a Symbol value. If such an explicitly specified value starts with the prefix "get " or "set " and the function for which it is specified is a get or set accessor function of a built-in property, the value without the prefix is passed to the name parameter, and the value "get" or "set" (respectively) is passed to the prefix parameter when calling CreateBuiltinFunction.
  // Unless otherwise specified, the "name" property of a built-in function object has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
  // .name = DataProperty { ... }
  //
  // Every other data property described in clauses 19 through 28 and in Annex B.2 has the attributes { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true } unless otherwise specified.
  // .data = DataProperty { ... }
  //
  // Every accessor property described in clauses 19 through 28 and in Annex B.2 has the attributes { [[Enumerable]]: false, [[Configurable]]: true } unless otherwise specified. If only a get accessor function is described, the set accessor function is the default value, undefined. If only a set accessor is described the get accessor is the default value, undefined.
  // .accessor = AccessorProperty { ... }

  // intrinsics
  private lazy val intrinsics: Map[String, Struct] = errors ++ Map(
    // TODO "print" -> Struct(
    //   typeName = "BuiltinFunctionObject",
    //   imap = List(
    //     "Extensible" -> Bool(true),
    //     "Prototype" -> intrAddr("Function.prototype"),
    //     "Code" -> intrClo("HostPrint"),
    //   ),
    //   nmap = List(
    //     "length" -> DataProperty(Number(0.0), F, F, T),
    //   ),
    // ),
    "Object" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Object"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("Object"), F, F, T),
        "prototype" -> DataProperty(intrAddr("Object.prototype"), F, F, F),
      ),
    ),
    "Object.prototype" -> Struct(
      typeName = "ImmutablePrototypeExoticObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> Null,
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Object"), T, F, T),
      ),
    ),
    "Function" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Function"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("Function"), F, F, T),
        "prototype" -> DataProperty(intrAddr("Function.prototype"), F, F, F),
      ),
    ),
    "Function.prototype" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
        "Code" -> intrClo("Function.prototype"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), F, F, T),
        "name" -> DataProperty(Str(""), F, F, T),
        "constructor" -> DataProperty(intrAddr("Function"), T, F, T),
        // TODO "caller" -> AccessorProperty(
        //   intrAddr("%ThrowTypeError%"),
        //   intrAddr("%ThrowTypeError%"),
        //   F,
        //   T,
        // ),
        // TODO "arguments" -> AccessorProperty(
        //   intrAddr("%ThrowTypeError%"),
        //   intrAddr("%ThrowTypeError%"),
        //   F,
        //   T,
        // ),
        "@hasInstance" -> DataProperty(
          intrAddr("Function.prototype[@@hasInstance]"),
          F,
          F,
          F,
        ),
      ),
    ),
    "Function.prototype[@@hasInstance]" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Function.prototype[@@hasInstance]"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("[Symbol.hasInstance]"), F, F, T),
      ),
    ),
    "%ThrowTypeError%" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(false),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("%ThrowTypeError%"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), F, F, F),
        "name" -> DataProperty(Str("%ThrowTypeError%"), F, F, F),
      ),
    ),
    "Boolean" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Boolean"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "prototype" -> DataProperty(intrAddr("Boolean.prototype"), F, F, F),
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
        "constructor" -> DataProperty(intrAddr("Boolean"), T, F, T),
      ),
    ),
    "Symbol" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Symbol"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "asyncIterator" -> DataProperty(
          intrAddr("Symbol.asyncIterator"),
          F,
          F,
          F,
        ),
        "hasInstance" -> DataProperty(intrAddr("Symbol.hasInstance"), F, F, F),
        "isConcatSpreadable" -> DataProperty(
          intrAddr("Symbol.isConcatSpreadable"),
          F,
          F,
          F,
        ),
        "iterator" -> DataProperty(intrAddr("Symbol.iterator"), F, F, F),
        "match" -> DataProperty(intrAddr("Symbol.match"), F, F, F),
        "matchAll" -> DataProperty(intrAddr("Symbol.matchAll"), F, F, F),
        "replace" -> DataProperty(intrAddr("Symbol.replace"), F, F, F),
        "search" -> DataProperty(intrAddr("Symbol.search"), F, F, F),
        "species" -> DataProperty(intrAddr("Symbol.species"), F, F, F),
        "split" -> DataProperty(intrAddr("Symbol.split"), F, F, F),
        "toPrimitive" -> DataProperty(intrAddr("Symbol.toPrimitive"), F, F, F),
        "toStringTag" -> DataProperty(intrAddr("Symbol.toStringTag"), F, F, F),
        "unscopables" -> DataProperty(intrAddr("Symbol.unscopables"), F, F, F),
        "length" -> DataProperty(Number(0.0), F, F, T),
        "prototype" -> DataProperty(intrAddr("Symbol.prototype"), F, F, F),
      ),
    ),
    "Symbol.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Symbol"), T, F, T),
        "description" -> AccessorProperty(
          intrAddr("Symbol.prototype.description"),
          U,
          F,
          T,
        ),
        "@toStringTag" -> DataProperty(Str("Symbol"), F, F, T),
        "@toPrimitive" -> DataProperty(
          intrAddr("Symbol.prototype[@@toPrimitive]"),
          F,
          F,
          T,
        ),
      ),
    ),
    "Symbol.prototype[@@toPrimitive]" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Symbol.prototype[@@toPrimitive]"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("[Symbol.toPrimitive]"), F, F, T),
      ),
    ),
    "Symbol.prototype.description" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("get:Symbol.prototype.description"),
      ),
      nmap = List(
        "name" -> DataProperty(Str("get description"), F, F, T),
        "length" -> DataProperty(Number(0.0), F, F, T),
      ),
    ),
    "Error" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Error"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("Error.prototype"), F, F, F),
      ),
    ),
    "Error.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Error"), T, F, T),
        "message" -> DataProperty(Str(""), T, F, T),
        "name" -> DataProperty(Str("Error"), T, F, T),
      ),
    ),
    "Number" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Number"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "EPSILON" -> DataProperty(Number(math.ulp(1.0)), F, F, F),
        "MAX_SAFE_INTEGER" -> DataProperty(Number(9007199254740991.0), F, F, F),
        "MAX_VALUE" -> DataProperty(Number(Double.MaxValue), F, F, F),
        "MIN_SAFE_INTEGER" ->
        DataProperty(Number(-9007199254740991.0), F, F, F),
        "MIN_VALUE" -> DataProperty(Number(Double.MinPositiveValue), F, F, F),
        "NaN" -> DataProperty(Number(Double.NaN), F, F, F),
        "NEGATIVE_INFINITY" -> DataProperty(
          Number(Double.NegativeInfinity),
          F,
          F,
          F,
        ),
        "parseFloat" -> DataProperty(intrAddr("parseFloat"), T, F, T),
        "parseInt" -> DataProperty(intrAddr("parseInt"), T, F, T),
        "POSITIVE_INFINITY" -> DataProperty(
          Number(Double.PositiveInfinity),
          F,
          F,
          F,
        ),
        "prototype" -> DataProperty(intrAddr("Number.prototype"), F, F, F),
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
        "constructor" -> DataProperty(intrAddr("Number"), T, F, T),
      ),
    ),
    "BigInt" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("BigInt"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("BigInt.prototype"), F, F, F),
        "asIntN" -> DataProperty(intrAddr("BigInt.asIntN"), T, F, T),
        "asUintN" -> DataProperty(intrAddr("BigInt.asUintN"), T, F, T),
      ),
    ),
    "BigInt.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("BigInt"), T, F, T),
        "@toStringTag" -> DataProperty(Str("BigInt"), F, F, T),
      ),
    ),
    "String" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("String"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("String.prototype"), F, F, F),
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
        "length" -> DataProperty(Number(0.0), F, F, F),
        "constructor" -> DataProperty(intrAddr("String"), T, F, T),
      ),
    ),
    "StringIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "@toStringTag" -> DataProperty(Str("String Iterator"), F, F, T),
      ),
    ),
    "Array" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Array"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "prototype" -> DataProperty(intrAddr("Array.prototype"), F, F, F),
        "@species" -> AccessorProperty(
          intrAddr("Array[@@species]"),
          U,
          F,
          T,
        ),
      ),
    ),
    "Array[@@species]" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("get:Array[@@species]"),
      ),
      nmap = List(
        "name" -> DataProperty(Str("get [Symbol.species]"), F, F, T),
        "length" -> DataProperty(Number(0.0), F, F, T),
      ),
    ),
    "Array.prototype" -> Struct(
      typeName = "ArrayExoticObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), T, F, F),
        "constructor" -> DataProperty(intrAddr("Array"), T, F, T),
        "@iterator" -> DataProperty(
          intrAddr("Array.prototype.values"),
          T,
          F,
          T,
        ),
        "@unscopables" -> DataProperty(
          intrAddr("Array.prototype[@@unscopables]"),
          F,
          F,
          T,
        ),
      ),
    ),
    "Array.prototype[@@unscopables]" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> Null,
      ),
      nmap = List(
        "copyWithin" -> DataProperty(Bool(true), T, T, T),
        "entries" -> DataProperty(Bool(true), T, T, T),
        "fill" -> DataProperty(Bool(true), T, T, T),
        "find" -> DataProperty(Bool(true), T, T, T),
        "findIndex" -> DataProperty(Bool(true), T, T, T),
        "flat" -> DataProperty(Bool(true), T, T, T),
        "flatMap" -> DataProperty(Bool(true), T, T, T),
        "includes" -> DataProperty(Bool(true), T, T, T),
        "keys" -> DataProperty(Bool(true), T, T, T),
        "values" -> DataProperty(Bool(true), T, T, T),
      ),
    ),
    "ArrayIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "@toStringTag" -> DataProperty(Str("Array Iterator"), F, F, T),
      ),
    ),
    "Map" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Map"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("Map.prototype"), F, F, F),
        "@species" -> AccessorProperty(intrAddr("Map[@@species]"), U, F, T),
      ),
    ),
    "Map[@@species]" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("get:Map[@@species]"),
      ),
      nmap = List(
        "name" -> DataProperty(Str("get [Symbol.species]"), F, F, T),
        "length" -> DataProperty(Number(0.0), F, F, T),
      ),
    ),
    "Map.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Map"), T, F, T),
        "size" -> AccessorProperty(intrAddr("Map.prototype.size"), U, F, T),
        "@iterator" -> DataProperty(intrAddr("Map.prototype.entries"), T, F, T),
        "@toStringTag" -> DataProperty(Str("Map"), F, F, T),
      ),
    ),
    "Map.prototype.size" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("get:Map.prototype.size"),
      ),
      nmap = List(
        "name" -> DataProperty(Str("get size"), F, F, T),
        "length" -> DataProperty(Number(0.0), F, F, T),
      ),
    ),
    "MapIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "@toStringTag" -> DataProperty(Str("Map Iterator"), F, F, T),
      ),
    ),
    "Set" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Set"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("Set.prototype"), F, F, F),
        "@species" -> AccessorProperty(intrAddr("Set[@@species]"), U, F, T),
      ),
    ),
    "Set[@@species]" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("get:Set[@@species]"),
      ),
      nmap = List(
        "name" -> DataProperty(Str("get [Symbol.species]"), F, F, T),
        "length" -> DataProperty(Number(0.0), F, F, T),
      ),
    ),
    "Set.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Set"), T, F, T),
        "keys" -> DataProperty(intrAddr("Set.prototype.values"), T, F, T),
        "size" -> AccessorProperty(intrAddr("Set.prototype.size"), U, F, T),
        "@iterator" -> DataProperty(intrAddr("Set.prototype.values"), T, F, T),
        "@toStringTag" -> DataProperty(Str("Set"), F, F, T),
      ),
    ),
    "Set.prototype.size" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("get:Set.prototype.size"),
      ),
      nmap = List(
        "name" -> DataProperty(Str("get size"), F, F, T),
        "length" -> DataProperty(Number(0.0), F, F, T),
      ),
    ),
    "SetIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "@toStringTag" -> DataProperty(Str("Set Iterator"), F, F, T),
      ),
    ),
    "WeakMap" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("WeakMap"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("WeakMap.prototype"), F, F, F),
      ),
    ),
    "WeakMap.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("WeakMap"), T, F, T),
        "@toStringTag" -> DataProperty(Str("WeakMap"), F, F, T),
      ),
    ),
    "WeakSet" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("WeakSet"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("WeakSet.prototype"), F, F, F),
      ),
    ),
    "WeakSet.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("WeakSet"), T, F, T),
        "@toStringTag" -> DataProperty(Str("WeakSet"), F, F, T),
      ),
    ),
    "IteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(),
    ),
    "AsyncIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(),
    ),
    "AsyncFromSyncIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("AsyncIteratorPrototype"),
      ),
      nmap = List(),
    ),
    "GeneratorFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Code" -> intrClo("GeneratorFunction"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
        "Extensible" -> Bool(true),
        "ScriptOrModule" -> Null,
        "Realm" -> realmAddr,
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("GeneratorFunction"), F, F, T),
        "prototype" -> DataProperty(
          intrAddr("GeneratorFunction.prototype"),
          F,
          F,
          F,
        ),
      ),
    ),
    "GeneratorFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("GeneratorFunction"), F, F, T),
        "prototype" -> DataProperty(intrAddr("Generator.prototype"), F, F, T),
        "@toStringTag" -> DataProperty(Str("GeneratorFunction"), F, F, T),
      ),
    ),
    "Generator.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(
          intrAddr("GeneratorFunction.prototype"),
          F,
          F,
          T,
        ),
        "@toStringTag" -> DataProperty(Str("Generator"), F, F, T),
      ),
    ),
    "AsyncGeneratorFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Code" -> intrClo("AsyncGeneratorFunction"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
        "Extensible" -> Bool(true),
        "ScriptOrModule" -> Null,
        "Realm" -> realmAddr,
      ),
      nmap = List(
        "name" -> DataProperty(Str("AsyncGeneratorFunction"), F, F, T),
        "length" -> DataProperty(Number(1.0), F, F, T),
        "prototype" -> DataProperty(
          intrAddr("AsyncGeneratorFunction.prototype"),
          F,
          F,
          F,
        ),
      ),
    ),
    "AsyncGeneratorFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(
          intrAddr("AsyncGeneratorFunction"),
          F,
          F,
          T,
        ),
        "prototype" -> DataProperty(
          intrAddr("AsyncGenerator.prototype"),
          F,
          F,
          T,
        ),
        "@toStringTag" -> DataProperty(Str("AsyncGeneratorFunction"), F, F, T),
      ),
    ),
    "AsyncGenerator.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("AsyncIteratorPrototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(
          intrAddr("AsyncGeneratorFunction.prototype"),
          F,
          F,
          T,
        ),
        "@toStringTag" -> DataProperty(Str("AsyncGenerator"), F, F, T),
      ),
    ),
    "Promise" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("Promise"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("Promise"), F, F, T),
        "prototype" -> DataProperty(intrAddr("Promise.prototype"), F, F, F),
        "@species" -> AccessorProperty(
          intrAddr("Promise[@@species]"),
          U,
          F,
          T,
        ),
      ),
    ),
    "Promise[@@species]" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> intrClo("get:Promise[@@species]"),
      ),
      nmap = List(
        "name" -> DataProperty(Str("get [Symbol.species]"), F, F, T),
        "length" -> DataProperty(Number(0.0), F, F, T),
      ),
    ),
    "Promise.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Promise"), T, F, T),
        "@toStringTag" -> DataProperty(Str("Promise"), F, F, T),
      ),
    ),
    "AsyncFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Code" -> intrClo("AsyncFunction"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
        "Extensible" -> Bool(true),
        "ScriptOrModule" -> Null,
        "Realm" -> realmAddr,
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("AsyncFunction"), F, F, T),
        "prototype" -> DataProperty(
          intrAddr("AsyncFunction.prototype"),
          F,
          F,
          F,
        ),
      ),
    ),
    "AsyncFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("AsyncFunction"), F, F, T),
        "@toStringTag" -> DataProperty(Str("AsyncFunction"), F, F, T),
      ),
    ),
    "Number.prototype.toString" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "String.fromCharCode" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "String.fromCodePoint" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "String.prototype.concat" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype.concat" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype.push" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype.unshift" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "Object.assign" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "AggregateError" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Error"),
        "Construct" -> clo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(
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
        "constructor" -> DataProperty(intrAddr("AggregateError"), T, F, T),
        "message" -> DataProperty(Str(""), T, F, T),
        "name" -> DataProperty(Str("AggregateError"), T, F, T),
      ),
    ),
    "ForInIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(),
    ),
  )

  private def errList: List[String] = List(
    "EvalError",
    "RangeError",
    "ReferenceError",
    "SyntaxError",
    "TypeError",
    "URIError",
  )

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
          "Prototype" -> intrAddr("Error"),
          "Code" -> intrClo(errName),
          "Construct" -> clo("BuiltinFunctionObject.Construct"),
        ),
        nmap = List(
          "name" -> DataProperty(Str(errName), F, F, T),
          "prototype" -> DataProperty(
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
          "constructor" -> DataProperty(intrAddr(errName), T, F, T),
          "message" -> DataProperty(Str(""), T, F, T),
          "name" -> DataProperty(Str(errName), T, F, T),
        ),
      ),
    )

}
