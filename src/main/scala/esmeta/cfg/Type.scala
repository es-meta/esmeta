package esmeta.cfg

import esmeta.cfg.util.*

// TODO CFG types
case class Type(name: String) extends CFGElem
object Type extends Parser.From[Type]
