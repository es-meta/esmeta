package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.util.BaseUtils.*

/** CFG elements */
trait CFGElem {
  override def toString: String = toString(true, false)

  /** stringify with options */
  def toString(detail: Boolean = true, location: Boolean = false): String =
    val stringifier = CFGElem.getStringifier(detail, location)
    import stringifier.elemRule
    stringify(this)

  /** string of flow information */
  def flowString: String = FlowStringifier(this)

  /** fingerprint for validation */
  def fingerprint: String = sha512Hash(this.flowString)
}
object CFGElem {
  val getStringifier =
    cached[(Boolean, Boolean), Stringifier] { Stringifier(_, _) }
}
