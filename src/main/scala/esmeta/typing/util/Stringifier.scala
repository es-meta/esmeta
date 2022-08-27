package esmeta.typing.util

import esmeta.LINE_SEP
import esmeta.typing.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for types */
object Stringifier {
  // elements
  given elemRule: Rule[TyElem] = (app, elem) =>
    elem match
      case elem: Ty => typeRule(app, elem)

  // types
  given typeRule: Rule[Ty] = (app, ty) =>
    ty match
      case UnknownT(name) => app >> name
      case _              => ??? // TODO
}
