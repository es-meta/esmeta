package esmeta.ai.domain.grammar

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.Grammar

/** abstract grammar goal symbol domain */
trait Domain
  extends domain.Domain[Grammar]
  with Prunable[Grammar]
  with Meetable[Grammar]
