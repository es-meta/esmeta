package esmeta.typing

import esmeta.typing.util.*
import esmeta.util.BaseUtils.*

/** type elements */
trait TyElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}
