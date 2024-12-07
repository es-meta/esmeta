package esmeta.injector

import esmeta.injector.util.*
import esmeta.util.BaseUtils.*

/** Injector elements */
trait InjectorElem {
  override def toString: String = toString()

  /** stringify with options */
  def toString(detail: Boolean = false): String = {
    val stringifier = InjectorElem.getStringifier(detail)
    import stringifier.elemRule
    stringify(this)
  }
}
object InjectorElem {
  val getStringifier =
    cached[Boolean, Stringifier] { Stringifier(_) }
}
