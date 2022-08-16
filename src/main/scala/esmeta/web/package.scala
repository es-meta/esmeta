package esmeta.web

import esmeta.interpreter.Debugger
import esmeta.cfg.CFG
import esmeta.js.Initialize

def debugger: Debugger = _debugger.get
def initDebugger(cfg: CFG, sourceText: String): Unit =
  val cachedAst = cfg.jsParser("Script", Nil).from(sourceText)
  _debugger = Some(Debugger(Initialize(cfg, sourceText, Some(cachedAst))))
private var _debugger: Option[Debugger] = None
