package esmeta.interp

import esmeta.util.BaseUtils.*

/** Interp elements */
trait InterpElem {
  override def toString: String = toString(true, false)

  /** stringify with options */
  def toString(detail: Boolean = true, location: Boolean = false): String = {
    val stringifier = InterpElem.getStringifier(detail, location)
    import stringifier.elemRule
    stringify(this)
  }
}
object InterpElem {
  val getStringifier =
    cached[(Boolean, Boolean), Stringifier] { new Stringifier(_, _) }
}
