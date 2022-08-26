package esmeta.typing

import esmeta.typing.util.*
import esmeta.typing.util.Stringifier.Target
import esmeta.util.BaseUtils.*

/** Type elements */
trait TypeElem {
  override def toString: String = toString(Target.Lang)

  /** stringify with options */
  def toString(target: Target = Target.Lang): String =
    val stringifier = TypeElem.getStringifier(target)
    import stringifier.elemRule
    stringify(this)
}
object TypeElem {
  val getStringifier = cached[Target, Stringifier] { Stringifier(_) }
}
