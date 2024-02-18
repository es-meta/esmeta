package esmeta.util

import esmeta.util.BaseUtils.*

trait DoubleEquals {
  val double: Double
  override def equals(that: Any): Boolean = that match {
    case that: DoubleEquals =>
      (this.getClass eq that.getClass) &&
      doubleEquals(this.double, that.double)
    case _ => false
  }
}
