package esmeta.web

import esmeta.cfg.CFG

def debugger: Debugger = _debugger.get
def initDebugger(cfg: CFG, sourceText: String): Unit =
  _debugger = Some(Debugger(cfg.init.from(sourceText)))
def initDebugger(debugger: Debugger): Unit =
  _debugger = Some(debugger)
private var _debugger: Option[Debugger] = None

/** web server host */
val ESMETA_HOST = sys.env.getOrElse("ESMETA_HOST", "localhost")
