package esmeta.ai.domain.part

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.Addr

/** abstract address partition domain */
trait Domain extends domain.Domain[Part] {

  /** abstraction functions for an original address */
  def alpha(addr: Addr): Elem = alpha(Part.from(addr))
}
