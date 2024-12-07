package esmeta.injector

import esmeta.injector.util.*
import esmeta.util.BaseUtils.*

/** Injector elements */
trait InjectorElem {
  override def toString: String = toString()

  /** stringify with options */
  def toString(
    detail: Boolean = true,
    location: Boolean = false,
  ): String = {
    val stringifier = InjectorElem.getStringifier(detail, location)
    import stringifier.elemRule
    stringify(this)
  }
}
object InjectorElem {
  val getStringifier =
    cached[(Boolean, Boolean), Stringifier] { Stringifier(_, _) }
}
