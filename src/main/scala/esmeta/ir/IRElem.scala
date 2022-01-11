package esmeta.ir

import esmeta.util.BaseUtils.*

/** IR elements */
trait IRElem {
  override def toString: String = toString(true)

  /** stringify with options */
  def toString(detail: Boolean = true): String = {
    val stringifier = IRElem.getStringifier(detail)
    import stringifier.elemRule
    stringify(this)
  }
}
object IRElem {
  val getStringifier = cached(new Stringifier(_))
}
