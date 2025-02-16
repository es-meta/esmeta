package esmeta.util

import esmeta.util.BaseUtils.*

trait Printable[T: Appender.Rule] { self: T =>
  override def toString: String = stringify(this)
}
