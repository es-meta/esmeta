package esmeta.spec

import esmeta.util.BaseUtils._

/** specification elements */
trait SpecElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}
