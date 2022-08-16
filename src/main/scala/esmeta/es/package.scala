package esmeta.es

import esmeta.es.util.*
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.*

/** ECMAScript elements */
trait ESElem {
  override def toString: String = toString()

  /** stringify with options */
  def toString(
    detail: Boolean = true,
    location: Boolean = false,
    grammar: Option[Grammar] = None,
  ): String = {
    val stringifier = ESElem.getStringifier(detail, location, grammar)
    import stringifier.elemRule
    stringify(this)
  }
}
object ESElem {
  val getStringifier =
    cached[(Boolean, Boolean, Option[Grammar]), Stringifier] {
      Stringifier(_, _, _)
    }
}

/** timeout */
val TIMEOUT = 10
