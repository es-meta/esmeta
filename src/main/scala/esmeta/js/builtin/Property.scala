package esmeta.js.builtin

import esmeta.interp.*
import scala.collection.mutable.{Map => MMap}

// properties
sealed trait Property:
  def toObject: MapObj

// TODO
given Option[TypeModel] = None

// data properties
case class DataProperty(
  value: PureValue,
  writable: Boolean,
  enumerable: Boolean,
  configurable: Boolean,
) extends Property:
  def toObject: MapObj = MapObj("PropertyDescriptor")(
    Str("Value") -> value,
    Str("Writable") -> Bool(writable),
    Str("Enumerable") -> Bool(enumerable),
    Str("Configurable") -> Bool(configurable),
  )

// accessor properties
case class AccessorProperty(
  get: NamedAddr,
  set: NamedAddr,
  enumerable: Boolean,
  configurable: Boolean,
) extends Property:
  def toObject: MapObj = MapObj("PropertyDescriptor")(
    Str("Get") -> get,
    Str("Set") -> set,
    Str("Enumerable") -> Bool(enumerable),
    Str("Configurable") -> Bool(configurable),
  )
