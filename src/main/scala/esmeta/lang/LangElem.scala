package esmeta.lang

import esmeta.util.BaseUtils.*

/** Lang elements */
trait LangElem {
  override def toString: String = toString(true, false)

  /** stringify with options */
  def toString(detail: Boolean = true, location: Boolean = false): String = {
    val stringifier = LangElem.getStringifier(detail, location)
    import stringifier.elemRule
    stringify(this)
  }
}
object LangElem {
  val getStringifier =
    cached[(Boolean, Boolean), Stringifier] { new Stringifier(_, _) }
}
