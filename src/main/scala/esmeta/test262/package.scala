package esmeta.test262

import esmeta.test262.util.*
import esmeta.util.BaseUtils.*

/** Test262 elements */
trait Test262Elem {
  override def toString: String = toString()

  /** stringify with options */
  def toString(
    detail: Boolean = false,
  ): String = {
    val stringifier = Test262Elem.getStringifier(detail)
    import stringifier.elemRule
    stringify(this)
  }
}
object Test262Elem {
  val getStringifier = cached[Boolean, Stringifier] { Stringifier(_) }
}
