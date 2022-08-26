package esmeta.analyzer.domain.cont

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Cont

/** abstract continuation domain */
trait Domain extends domain.Domain[ACont] {

  /** abstraction functions for an original continuation */
  def alpha(cont: Cont): Elem = alpha(ACont.from(cont))
}
