package esmeta.interp

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for Interp */
case class Stringifier(detail: Boolean) {
  // elements
  given elemRule: Rule[InterpElem] = (app, elem) => ???
}
