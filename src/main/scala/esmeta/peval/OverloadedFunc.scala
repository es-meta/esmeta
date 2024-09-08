package esmeta.peval

import esmeta.cfg.{Func => CfgFunc}
import esmeta.ir.{Func => IRFunc}
import esmeta.state.{RecordObj}

type OverloadingCond = RecordObj // ad-hoc
case class OverloadedFunc(cond: OverloadingCond, overloadedName: String)
case class OverloadedIRFunc(cond: OverloadingCond, func: IRFunc)
