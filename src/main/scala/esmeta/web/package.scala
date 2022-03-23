package esmeta.web

import esmeta.interp.util.Debugger
import esmeta.cfg.CFG
import esmeta.js.Initialize

def debugger: Debugger = _debugger.get
def initDebugger(cfg: CFG, sourceText: String): Unit =
  // TODO add initial breakpoints
  _debugger = Some(Debugger(Initialize(cfg, sourceText)))
private var _debugger: Option[Debugger] = None
