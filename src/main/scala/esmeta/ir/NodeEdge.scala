package esmeta.ir

import esmeta.cfg.Node

/** edge to enclosing CFG nodes */
trait NodeEdge:
  /** edge to enclosing CFG nodes */
  var cfgNode: Option[Node] = None
