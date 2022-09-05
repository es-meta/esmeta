package esmeta.analyzer

import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** abstract transfer function for type analysis */
class TypeTransfer(sem: AbsSemantics) extends AbsTransfer(sem) {

  /** loading monads */
  import AbsState.monad.*
}
