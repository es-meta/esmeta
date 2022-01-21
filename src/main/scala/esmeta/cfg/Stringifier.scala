package esmeta.cfg

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for CFG */
case class Stringifier(detail: Boolean) {
  // elements
  given elemRule: Rule[CFGElem] = (app, elem) => ???
}
