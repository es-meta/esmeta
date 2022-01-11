package esmeta.lang

import esmeta.util.BaseUtils.*

/** Lang elements */
trait LangElem {
  override def toString: String = toString(true)

  /** stringify with options */
  def toString(detail: Boolean = true): String = {
    val stringifier = LangElem.getStringifier(detail)
    import stringifier.elemRule
    stringify(this)
  }
}
object LangElem {
  val getStringifier = cached(new Stringifier(_))
}
