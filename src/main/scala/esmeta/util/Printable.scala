package esmeta.util

import esmeta.util.BaseUtils.*

trait Printable[T](using Appender.Rule[T]) { self: T =>
  override def toString: String = stringify(this)
}
