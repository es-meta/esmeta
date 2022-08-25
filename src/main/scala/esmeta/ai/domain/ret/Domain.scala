package esmeta.ai.domain.ret

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract return value domain */
trait Domain extends domain.Domain[(AValue, State)]
