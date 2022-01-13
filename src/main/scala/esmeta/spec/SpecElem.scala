package esmeta.spec

import esmeta.util.BaseUtils.*

/** specification elements */
trait SpecElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}
