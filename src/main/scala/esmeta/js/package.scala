package esmeta.js

import esmeta.js.util.*
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.*

/** JavaScript elements */
trait JSElem {
  override def toString: String = toString()

  /** stringify with options */
  def toString(
    detail: Boolean = true,
    location: Boolean = false,
    grammar: Option[Grammar] = None,
  ): String = {
    val stringifier = JSElem.getStringifier(detail, location, grammar)
    import stringifier.elemRule
    stringify(this)
  }
}
object JSElem {
  val getStringifier =
    cached[(Boolean, Boolean, Option[Grammar]), Stringifier] {
      new Stringifier(_, _, _)
    }
}
