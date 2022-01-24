package esmeta.interp

import esmeta.util.BaseUtils.*

/** Interp elements */
trait InterpElem {
  override def toString: String = toString(true)

  /** stringify with options */
  def toString(detail: Boolean = true): String = {
    val stringifier = InterpElem.getStringifier(detail)
    import stringifier.elemRule
    stringify(this)
  }
}
object InterpElem {
  val getStringifier = cached(new Stringifier(_))
}
