package esmeta.lang

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

case class Stringifier(detail: Boolean) {
  // elements
  given elemRule: Rule[LangElem] = (app, elem) => ???
}
