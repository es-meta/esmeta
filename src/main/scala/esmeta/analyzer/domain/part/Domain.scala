package esmeta.analyzer.domain.part

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Addr

/** abstract address partition domain */
trait Domain extends domain.Domain[Part] {

  /** abstraction functions for an original address */
  def alpha(addr: Addr): Elem = alpha(Part.from(addr))
}
