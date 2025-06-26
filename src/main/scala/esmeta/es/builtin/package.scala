package esmeta.es.builtin

import esmeta.state.*
import esmeta.spec.*
import esmeta.cfg.CFG
import esmeta.ty.*
import scala.collection.mutable.{Map => MMap}

/** predefined shortcuts */
val T = true
val F = false
val U = Undef

/** predefined constants */
val INTRINSICS = "INTRINSICS"
val GLOBAL = "GLOBAL"
val SYMBOL = "SYMBOL"
val MATH_PI = "MATH_PI"
val AGENT_RECORD = "AGENT_RECORD"
val AGENT_SIGNIFIER = "AGENT_SIGNIFIER"
val CANDIDATE_EXECUTION = "CANDIDATE_EXECUTION"
val KEPT_ALIVE = "KEPT_ALIVE"
val REALM = "REALM"
val EXECUTION_STACK = "EXECUTION_STACK"
val HOST_DEFINED = "HOST_DEFINED"
val JOB_QUEUE = "JOB_QUEUE"
val PRIMITIVE = "PRIMITIVE"
val SOURCE_TEXT = "SOURCE_TEXT"
val SYMBOL_REGISTRY = "SYMBOL_REGISTRY"
val INNER_CODE = "__CODE__"
val INNER_MAP = "__MAP__"
val PRIVATE_ELEMENTS = "PrivateElements"
val DESCRIPTOR = "DESCRIPTOR"
val UNDEF_TYPE = "Undefined"
val NULL_TYPE = "Null"
val BOOL_TYPE = "Boolean"
val STRING_TYPE = "String"
val SYMBOL_TYPE = "Symbol"
val NUMBER_TYPE = "Number"
val BIGINT_TYPE = "BigInt"
val OBJECT_TYPE = "Object"

/** not yet supported objects */
val yets: Map[String, ValueTy] =
  Map(
    // 21. Numbers and Dates
    "Date" -> ConstructorT,
    // 22. Text Processing
    "RegExp" -> ConstructorT,
    // 23. Indexed Collections
    "TypedArray" -> ConstructorT,
    "Int8Array" -> ConstructorT,
    "Uint8Array" -> ConstructorT,
    "Uint8ClampedArray" -> ConstructorT,
    "Int16Array" -> ConstructorT,
    "Uint16Array" -> ConstructorT,
    "Int32Array" -> ConstructorT,
    "Uint32Array" -> ConstructorT,
    "BigInt64Array" -> ConstructorT,
    "BigUint64Array" -> ConstructorT,
    "Float32Array" -> ConstructorT,
    "Float64Array" -> ConstructorT,
    // 25. Structured Data
    "ArrayBuffer" -> ConstructorT,
    "SharedArrayBuffer" -> ConstructorT,
    "DataView" -> ConstructorT,
    "Atomics" -> ObjectT,
    "JSON" -> ObjectT,
    // 26. Managing Memory
    "WeakRef" -> ConstructorT,
    "FinalizationRegistry" -> ConstructorT,
    // test262
    "$262" -> ConstructorT,
    // ShadowRealm
    "ShadowRealm" -> ObjectT,
  )

/** not yet supported functions */
val yetFuncs: Set[String] = Set(
  "String.prototype.localeCompare",
  "String.prototype.toLocaleLowerCase",
  "Number.prototype.toLocaleString",
  "String.prototype.toUpperCase",
  "String.prototype.toLocaleUpperCase",
)

/** table id for
  * (table-6)[https://tc39.es/ecma262/#table-well-known-intrinsic-objects]
  */
val WELL_KNOWN_INTRINSICS = "table-well-known-intrinsic-objects"

/** table id for (table-1)[https://tc39.es/ecma262/#sec-well-known-symbols]
  */
val WELL_KNOWN_SYMBOLS = "table-well-known-symbols"

// address for the current realm
val realmAddr = NamedAddr(REALM)

/** intrinsics name */
def intrName(name: String): String = s"$INTRINSICS.$name"

/** intrinsics addr */
def intrAddr(name: String): NamedAddr = NamedAddr(intrName(name))

/** map name */
def mapName(name: String): String = s"$name.$INNER_MAP"

/** map addr */
def mapAddr(name: String): NamedAddr = NamedAddr(mapName(name))

/** PrivateElements name */
def elemsName(name: String): String = s"$name.$PRIVATE_ELEMENTS"

/** PrivateElements addr */
def elemsAddr(name: String): NamedAddr = NamedAddr(elemsName(name))

/** symbol name */
def symbolName(name: String): String = s"Symbol.$name"

/** symbol address */
def symbolAddr(name: String): NamedAddr = intrAddr(symbolName(name))

/** descriptor name */
def descName(name: String, key: String): String =
  if (key.startsWith("@@")) s"$DESCRIPTOR.$name[$key]"
  else s"$DESCRIPTOR.$name.$key"

/** descriptor address */
def descAddr(name: String, key: String): NamedAddr =
  NamedAddr(descName(name, key))

/** get map */
def getMapObjects(
  name: String,
  descBase: String,
  nmap: List[(String, Property)],
)(using CFG): Map[Addr, Obj] =
  var map = Map[Addr, Obj]()
  map += mapAddr(name) -> MapObj(nmap.map {
    case (k, _) => // handle symbol
      val key = if (k.startsWith("@@")) symbolAddr(k.drop(2)) else Str(k)
      key -> descAddr(descBase, k)
  })
  map ++= nmap.map { case (k, prop) => descAddr(descBase, k) -> prop.toObject }
  map
