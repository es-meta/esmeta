package esmeta.spec

import esmeta.spec.util.Stringifier
import esmeta.util.BaseUtils.stringify

/** specification elements */
trait SpecElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}
