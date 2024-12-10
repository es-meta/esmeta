package esmeta.lang.util

import esmeta.lang.Type
import esmeta.ty.Ty

// TODO temporary solution - use ty instead.
object LangTypeFromTy {
  def from(str: String): Type = Type(Ty.from(str))
}
