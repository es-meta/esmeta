package esmeta.js.builtin

import esmeta.state.*
import esmeta.spec.*
import esmeta.cfg.CFG
import scala.collection.mutable.{Map => MMap}

/** predefined shortcuts */
val T = true
val F = false
val U = Undef

/** predefined constants */
val INTRINSICS = "INTRINSICS"
val GLOBAL = "GLOBAL"
val SYMBOL = "SYMBOL"
val REALM = "REALM"
val EXECUTION_STACK = "EXECUTION_STACK"
val HOST_DEFINED = "HOST_DEFINED"
val JOB_QUEUE = "JOB_QUEUE"
val PRIMITIVE = "PRIMITIVE"
val SOURCE_TEXT = "SOURCE_TEXT"
val SYMBOL_REGISTRY = "SYMBOL_REGISTRY"
val SUBMAP = "SubMap"
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
val yets: Set[String] = Set(
  // 21. Numbers and Dates
  "Math",
  "Date",
  // 22. Text Processing
  "RegExp",
  // 23. Indexed Collections
  "TypedArray",
  "Int8Array",
  "Uint8Array",
  "Uint8ClampedArray",
  "Int16Array",
  "Uint16Array",
  "Int32Array",
  "Uint32Array",
  "BigInt64Array",
  "BigUint64Array",
  "Float32Array",
  "Float64Array",
  // 25. Structured Data
  "ArrayBuffer",
  "SharedArrayBuffer",
  "DataView",
  "Atomics",
  "JSON",
  // 26. Managing Memory
  "WeakRef",
  "FinalizationRegistry",
  // 28. Reflection
  "Reflect",
  "Proxy",
  // test262
  "$262",
  // ShadowRealm
  "ShadowRealm",
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

/** submap name */
def submapName(name: String): String = s"$name.$SUBMAP"

/** submap addr */
def submapAddr(name: String): NamedAddr = NamedAddr(submapName(name))

/** symbol name */
def symbolName(name: String): String = s"Symbol.$name"

/** symbol address */
def symbolAddr(name: String): NamedAddr = intrAddr(symbolName(name))

/** descriptor name */
def descName(name: String, key: String): String =
  if (key startsWith "@@") s"$DESCRIPTOR.$name[$key]"
  else s"$DESCRIPTOR.$name.$key"

/** descriptor address */
def descAddr(name: String, key: String): NamedAddr =
  NamedAddr(descName(name, key))

/** get submap */
def getSubmapObjects(
  name: String,
  descBase: String,
  nmap: List[(String, Property)],
)(using CFG): Map[Addr, Obj] =
  var map = Map[Addr, Obj]()
  map += submapAddr(name) -> MapObj(SUBMAP)(nmap.map {
    case (k, _) => // handle symbol
      val key = if k startsWith "@@" then symbolAddr(k.drop(2)) else Str(k)
      key -> descAddr(descBase, k)
  }: _*)
  map ++= nmap.map { case (k, prop) => descAddr(descBase, k) -> prop.toObject }
  map
