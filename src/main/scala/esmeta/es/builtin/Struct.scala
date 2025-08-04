package esmeta.es.builtin

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.state.*

/** builtin model structure */
case class Struct(
  typeName: String,
  imap: List[(String, Value)] = List(),
  nmap: List[(PropKey, PropDesc)] = List(),
) extends ESElem

/** property key */
enum PropKey extends ESElem:
  case Str(str: String)
  case Sym(sym: String)

// property descriptor
sealed trait PropDesc extends ESElem {

  /** convert to ir map object */
  def toObject(using CFG): RecordObj = this match
    case DataDesc(v, w, e, c) =>
      recordObj("PropertyDescriptor")(
        "Value" -> v,
        "Writable" -> Bool(w),
        "Enumerable" -> Bool(e),
        "Configurable" -> Bool(c),
      )
    case AccessorDesc(g, s, e, c) =>
      recordObj("PropertyDescriptor")(
        "Get" -> g,
        "Set" -> s,
        "Enumerable" -> Bool(e),
        "Configurable" -> Bool(c),
      )
}

// data properties
case class DataDesc(
  value: Value,
  writable: Boolean,
  enumerable: Boolean,
  configurable: Boolean,
) extends PropDesc

// accessor properties
case class AccessorDesc(
  get: Value,
  set: Value,
  enumerable: Boolean,
  configurable: Boolean,
) extends PropDesc
