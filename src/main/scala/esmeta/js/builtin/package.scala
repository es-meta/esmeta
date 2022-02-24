package esmeta.js.builtin

import esmeta.interp.*
import esmeta.spec.*
import esmeta.cfg.CFG
import scala.collection.mutable.{Map => MMap}

/** predefined constants */
val INTRINSICS = "INTRINSICS"
val GLOBAL = "GLOBAL"
val CONTEXT = "CONTEXT"
val REALM = "REALM"
val EXECUTION_STACK = "EXECUTION_STACK"
val HOST_DEFINED = "HOST_DEFINED"
val JOB_QUEUE = "JOB_QUEUE"
val PRIMITIVE = "PRIMITIVE"
val SOURCE_TEXT = "SOURCE_TEXT"
val SYMBOL_REGISTRY = "SYMBOL_REGISTRY"
val SUBMAP = "SubMap"
val SYMBOL = "SYMBOL"
val DESCRIPTOR = "DESCRIPTOR"
val UNDEF_TYPE = "Undefined"
val NULL_TYPE = "Null"
val BOOL_TYPE = "Boolean"
val STRING_TYPE = "String"
val SYMBOL_TYPE = "Symbol"
val NUMBER_TYPE = "Number"
val BIGINT_TYPE = "BigInt"
val OBJECT_TYPE = "Object"

/** table id for
  * (table-6)[https://tc39.es/ecma262/#table-well-known-intrinsic-objects]
  */
val WELL_KNOWN_INTRINSICS = "table-well-known-intrinsic-objects"

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
def symbolName(name: String): String = s"$SYMBOL.$name"

/** symbol address */
def symbolAddr(name: String): NamedAddr = NamedAddr(symbolName(name))

/** descriptor name */
def descName(name: String, key: String): String = s"$DESCRIPTOR.$name.$key"

/** descriptor address */
def descAddr(name: String, key: String): NamedAddr =
  NamedAddr(descName(name, key))

/** get submap */
def getSubmapObjects(
  name: String,
  nmap: List[(String, Property)],
)(using CFG): Map[Addr, Obj] =
  var map = Map[Addr, Obj]()
  map += submapAddr(name) -> MapObj(SUBMAP)(nmap.map {
    case (k, _) => // handle symbol
      val key = if k startsWith "@@" then symbolAddr(k.drop(2)) else Str(k)
      key -> descAddr(name, k)
  }: _*)
  map ++= nmap.map { case (k, prop) => descAddr(name, k) -> prop.toObject }
  map
