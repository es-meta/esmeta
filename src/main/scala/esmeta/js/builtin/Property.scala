package esmeta.js.builtin

import esmeta.interp.*
import scala.collection.mutable.{Map => MMap}

// properties
sealed trait Property

// data properties
case class DataProperty(
  value: PureValue,
  writable: Boolean,
  enumerable: Boolean,
  configurable: Boolean,
) extends Property

// accessor properties
case class AccessorProperty(
  get: NamedAddr,
  set: NamedAddr,
  enumerable: Boolean,
  configurable: Boolean,
) extends Property
