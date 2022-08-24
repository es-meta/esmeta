package esmeta.ai.domain.cont

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.Cont

/** abstract continuation domain */
trait Domain extends domain.Domain[ACont] {

  /** abstraction functions for an original continuation */
  def alpha(cont: Cont): Elem = alpha(ACont.from(cont))
}
