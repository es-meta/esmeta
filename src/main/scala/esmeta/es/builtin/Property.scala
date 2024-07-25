package esmeta.es.builtin

import esmeta.cfg.CFG
import esmeta.state.*
import scala.collection.mutable.{Map => MMap}

// properties
sealed trait Property {

  /** convert to ir map object */
  def toObject(using CFG): RecordObj = this match
    case DataProperty(v, w, e, c) =>
      RecordObj("PropertyDescriptor")(
        "Value" -> v,
        "Writable" -> Bool(w),
        "Enumerable" -> Bool(e),
        "Configurable" -> Bool(c),
      )
    case AccessorProperty(g, s, e, c) =>
      RecordObj("PropertyDescriptor")(
        "Get" -> g,
        "Set" -> s,
        "Enumerable" -> Bool(e),
        "Configurable" -> Bool(c),
      )

}

// data properties
case class DataProperty(
  value: PureValue,
  writable: Boolean,
  enumerable: Boolean,
  configurable: Boolean,
) extends Property

// accessor properties
case class AccessorProperty(
  get: PureValue,
  set: PureValue,
  enumerable: Boolean,
  configurable: Boolean,
) extends Property
