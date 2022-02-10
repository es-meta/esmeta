package esmeta.js

import esmeta.js.util.*
import esmeta.util.BaseUtils.*

/** JavaScript elements */
trait JsElem {
  override def toString: String = toString(true, false)

  /** stringify with options */
  def toString(detail: Boolean = true, location: Boolean = false): String = {
    val stringifier = JsElem.getStringifier(detail, location)
    import stringifier.elemRule
    stringify(this)
  }
}
object JsElem {
  val getStringifier =
    cached[(Boolean, Boolean), Stringifier] { new Stringifier(_, _) }
}
