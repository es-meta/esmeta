package esmeta.cfg

import esmeta.util.BaseUtils.*

/** CFG elements */
trait CFGElem {
  override def toString: String = toString(true)

  /** stringify with options */
  def toString(detail: Boolean = true): String = {
    val stringifier = CFGElem.getStringifier(detail)
    import stringifier.elemRule
    stringify(this)
  }
}
object CFGElem {
  val getStringifier = cached(new Stringifier(_))
}
