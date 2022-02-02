package esmeta.spec

import esmeta.spec.util.*
import esmeta.util.BaseUtils.*

/** specification elements */
trait SpecElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}
